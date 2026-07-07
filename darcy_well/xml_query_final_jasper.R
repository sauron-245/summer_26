############################################################
#  Darcy Well (Parish) — query DTS observations straight from Google Drive
#
#  this indexes the filenames, lets you pick the exact days/times you want, and
#  only downloads and parses those few files.
#
#  HOW TO RUN:
#   1. drive_auth() to connect.
#   2. Set FOLDER_ID and the depth offset below.
#   3. Build the index once, then edit the USER SETTINGS block and run.
###########################

library(googledrive)
library(xml2)
library(dplyr)
library(tibble)
library(lubridate)
library(stringr)
library(ggplot2)



# the google drive folder holding the darcy well xml files
FOLDER_ID <- "1wQGImxaQGcEA_156JJBwIDPLrG86JfZQ"

# length of fiber (m) before it enters the top of the well; depth = LAF - offset.
# set to 0 to start, adjust once you know where LAF = top-of-well sits.
TOP_OF_WELL_LAF <- 0

# plausible temperature window (°C) for dropping junk fiber points
TMP_MIN <- -20
TMP_MAX <- 120


# lists every xml in the drive folder and parses the timestamp out of each name
build_file_index <- function(folder_id) {
  items <- drive_find(q = sprintf("'%s' in parents", folder_id),
                      corpus = "allDrives")
  items <- items[grepl("\\.xml$", items$name, ignore.case = TRUE), ]
  
  tibble(name = items$name, id = items$id) %>%
    mutate(
      channel  = str_match(name, "channel\\s*([0-9]+)")[, 2],
      date_str = str_match(name, "_(\\d{8})_")[, 2],
      time_str = str_match(name, "_\\d{8}_(\\d{6})")[, 2],
      datetime = ymd_hms(paste0(date_str, time_str), tz = "UTC"),
      year = year(datetime), month = month(datetime), day = day(datetime),
      hour = hour(datetime), minute = minute(datetime)
    )
}


# reads a single downloaded xml into a tidy data frame of fiber points
parse_one_xml <- function(path) {
  d <- tryCatch(xml_ns_strip(read_xml(path)), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  
  t_start <- xml_text(xml_find_first(d, "//startDateTimeIndex"))
  curves  <- xml_find_all(d, "//logCurveInfo")
  col_names <- vapply(curves,
                      function(x) xml_text(xml_find_first(x, ".//mnemonic")),
                      character(1))
  
  nodes <- xml_find_all(d, "//data")
  if (length(nodes) == 0) return(NULL)
  m <- tryCatch(
    do.call(rbind, lapply(strsplit(trimws(xml_text(nodes)), ","), as.numeric)),
    warning = function(w) NULL, error = function(e) NULL)
  if (is.null(m)) return(NULL)
  if (ncol(m) != length(col_names)) col_names <- paste0("V", seq_len(ncol(m)))
  
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- col_names
  df$start_time <- t_start
  df
}

# downloads each selected file one at a time, parses it, discards the temp copy
fetch_and_parse <- function(selected) {
  if (nrow(selected) == 0) stop("no files selected — check your settings against what's in the folder")
  tmp <- tempfile(fileext = ".xml")
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  
  out <- vector("list", nrow(selected))
  for (i in seq_len(nrow(selected))) {
    ok <- tryCatch({
      drive_download(as_id(selected$id[i]), path = tmp, overwrite = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (ok) out[[i]] <- parse_one_xml(tmp)
    if (i %% 10 == 0 || i == nrow(selected))
      message("parsed ", i, " / ", nrow(selected))
  }
  bind_rows(out)
}


# drops bad temps and turns length-along-fiber into depth below top-of-well
add_depth <- function(df) {
  df %>%
    filter(TMP > TMP_MIN, TMP < TMP_MAX) %>%
    mutate(depth_m = LAF - TOP_OF_WELL_LAF,
           datetime = ymd_hms(start_time, tz = "UTC")) %>%
    filter(depth_m >= 0)          # keep only points at or below the well top
}



#  build the index ONCE per session (this is the slow-ish drive call)


index <- build_file_index(FOLDER_ID)
cat("indexed", nrow(index), "files, spanning",
    format(min(index$datetime, na.rm = TRUE)), "to",
    format(max(index$datetime, na.rm = TRUE)), "\n")


############################################################
#  USER SETTINGS — edit these to choose what data you want
############################################################


target_hour <- 12            # hour of day (UTC, 0-23) to sample near; 12 = noon

# pick ONE mode by setting it TRUE:
mode_day_of_month <- TRUE    # one observation per month, on a chosen day
mode_date_range   <- FALSE   # every day between two dates
mode_single_day   <- FALSE   # just one specific day

# settings for mode_day_of_month:
day_of_month <- 15           # e.g. 15 = the 15th of each month

# settings for mode_date_range:
range_start <- "2025-06-01"  # "YYYY-MM-DD"
range_end   <- "2025-06-30"

# settings for mode_single_day:
single_day  <- "2025-06-15"  # "YYYY-MM-DD"

############################################################
#  (you shouldn't need to edit below here)
############################################################


# builds the file selection from your settings above
pick_files <- function() {
  base <- index
  
  if (mode_day_of_month) {
    base <- base %>% filter(day == day_of_month)
  } else if (mode_date_range) {
    base <- base %>% filter(as_date(datetime) >= as_date(range_start),
                            as_date(datetime) <= as_date(range_end))
  } else if (mode_single_day) {
    base <- base %>% filter(as_date(datetime) == as_date(single_day))
  }
  
  # keep the one measurement nearest the target hour on each day
  base %>%
    mutate(date_only = as_date(datetime),
           mins_from_target = abs((hour * 60 + minute) - target_hour * 60)) %>%
    group_by(date_only) %>%
    slice_min(mins_from_target, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-date_only, -mins_from_target)
}

picks <- pick_files()

# quick check: what did you actually get?
cat("selected", nrow(picks), "files\n")
picks %>% mutate(date = as_date(datetime)) %>% distinct(date) %>% arrange(date) %>% print(n = 50)

# fetch and add depth
dat <- fetch_and_parse(picks) %>% add_depth()


# =====================================================================
#  plot: temperature vs depth, one line per selected observation
# =====================================================================

dat %>%
  mutate(label = format(datetime, "%Y-%m-%d")) %>%
  ggplot(aes(TMP, depth_m, group = start_time, color = label)) +
  geom_path(linewidth = 0.5) +
  scale_y_reverse() +
  labs(x = "Temperature (°C)", y = "Depth below well top (m)", color = "date",
       title = "Darcy Well — temperature vs depth") +
  theme_bw()
