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
############################################################

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
    mutate(
      datetime = ymd_hms(start_time, tz = "UTC"),
      label = format(datetime, "%Y-%m-%d %H:%M"),
      leg = if_else(LAF <= TURNAROUND_LAF, "down", "up"),
      depth_m = if_else(LAF <= TURNAROUND_LAF,
                        LAF - TOP_OF_WELL_LAF,
                        2 * TURNAROUND_LAF - LAF - TOP_OF_WELL_LAF),
      depth_ft = depth_m * 3.28084
    ) %>%
    filter(depth_m >= 0)          # keep only points at or below the well top
}





index <- build_file_index(FOLDER_ID)
cat("indexed", nrow(index), "files, spanning",
    format(min(index$datetime, na.rm = TRUE)), "to",
    format(max(index$datetime, na.rm = TRUE)), "\n")


############################################################
#  USER SETTINGS — edit these to choose what data you want
############################################################
### 6pm UTC is the ice test####
target_hour <- 22            # hour of day (UTC, 0-23) to sample near; 12 = noon UTC (6am CST)

# choose ONE mode: "day_of_month", "date_range", "single_day", or "time_window"
mode <- "time_window"

# for mode = "time_window":  every measurement between two times on one day
window_day        <- "2026-07-06"   # "YYYY-MM-DD"
window_start_hour <- 0              # start hour (UTC, 0-23)
window_end_hour   <- 23           # end hour (UTC, 0-23)

# for mode = "day_of_month":
day_of_month <- 15

# for mode = "date_range":
range_start <- "2026-07-01"
range_end   <- "2026-07-30"

# for mode = "single_day":
single_day  <- "2026-06-15"

############################################################
#  (you shouldn't need to edit below here)
############################################################

pick_files <- function() {
  base <- index
  
  if (mode == "day_of_month") {
    base <- base %>% filter(day == day_of_month)
  } else if (mode == "date_range") {
    base <- base %>% filter(as_date(datetime) >= as_date(range_start),
                            as_date(datetime) <= as_date(range_end))
  } else if (mode == "single_day") {
    base <- base %>% filter(as_date(datetime) == as_date(single_day))
  } else if (mode == "time_window") {
    # every measurement on one day between two hours — no thinning
    return(
      base %>%
        filter(as_date(datetime) == as_date(window_day),
               hour >= window_start_hour,
               hour <  window_end_hour) %>%
        arrange(datetime)
    )
  } else {
    stop("mode must be 'day_of_month', 'date_range', 'single_day', or 'time_window'")
  }
  
  # the other three modes thin to one observation per day near target_hour
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

# well geometry (should be fixed) 

TURNAROUND_LAF  <- 83.5   # lowest point of the well (cable turnaround), in meters along fiber
TOP_OF_WELL_LAF <- 9      # LAF where the cable enters the top of the well; depth = LAF - this
TMP_MIN <- -20            # drop physically impossible temperatures
TMP_MAX <- 120


# folds one profile: splits into down/up legs, converts each to true depth,
#  discards everything past the up-leg (the out-of-well junk)
fold_profile <- function(df) {
  well_bottom_laf <- TURNAROUND_LAF + (TURNAROUND_LAF - TOP_OF_WELL_LAF)  # end of up-leg
  
  df %>%
    filter(TMP > TMP_MIN, TMP < TMP_MAX,
           LAF >= TOP_OF_WELL_LAF, LAF <= well_bottom_laf) %>%
    mutate(
      datetime = ymd_hms(start_time, tz = "UTC"),
      label = format(datetime, "%Y-%m-%d %H:%M"),
      leg = if_else(LAF <= TURNAROUND_LAF, "down", "up"),
      depth_m = if_else(LAF <= TURNAROUND_LAF,
                        LAF - TOP_OF_WELL_LAF,                       # down: depth grows with LAF
                        2 * TURNAROUND_LAF - LAF - TOP_OF_WELL_LAF)  # up: mirrored back to top
    ) %>%
    filter(depth_m >= 0)
}


# build the folded data for whatever queried

folded <- fold_profile(dat)


############################################################
#  PLOT — down (solid) vs up (dashed), colored by observation
#  Each queried observation gets its own color; solid = cable going down,
#  dashed = cable coming back up. Overlapping solid/dashed = good symmetry.

plot_data <- folded %>% filter(leg == "down")

comparison_plot1 <- ggplot(data = plot_data,
                           mapping = aes(TMP, depth_ft, color = label, linetype = leg,
                                         group = interaction(start_time, leg))) +
  geom_path(linewidth = 0.6) +
  scale_y_reverse() +
  coord_cartesian(xlim = c(10, 12)) +
  scale_linetype_manual(values = c(down = "solid", up = "dashed"), guide = "none") +
  scale_color_viridis_d(option = "viridis") +
  labs(x = "Temperature (°C)", y = "Depth below well top (ft)",
       color = "observation (UTC)",
       title = "Darcy Observation Well — temperature vs depth") +
  theme_bw()

comparison_plot1

comparison_plot2 <- ggplot(data = plot_data,
                           mapping = aes(TMP, depth_ft, color = label, linetype = leg,
                                         group = interaction(start_time, leg))) +
  geom_path(linewidth = 0.6) +
  scale_y_reverse() +
  scale_linetype_manual(values = c(down = "solid", up = "dashed"), guide = "none") +
  scale_color_viridis_d(option = "viridis") +
  labs(x = "Temperature (°C)", y = "Depth below well top (ft)",
       color = "observation",
       title = "Darcy Observation Well — temperature vs depth") +
  theme_bw()

comparison_plot2
####pumping test dates march 19 - 27 and startup testing may 4-5####



