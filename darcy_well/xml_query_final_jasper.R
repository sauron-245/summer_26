############################################################
#  Darcy Well — SETUP (run this whole block ONCE per session)
#  Does the slow drive_auth + indexing, and defines all functions.
#  After this runs once, you never need it again until you restart R.
############################################################

library(googledrive)
library(xml2)
library(dplyr)
library(tibble)
library(lubridate)
library(stringr)
library(ggplot2)

# --- fixed settings & well geometry (defined up top so functions can use them) ---
FOLDER_ID       <- "1wQGImxaQGcEA_156JJBwIDPLrG86JfZQ"
TURNAROUND_LAF  <- 83.5   # lowest point of the well (cable turnaround), meters along fiber
TOP_OF_WELL_LAF <- 8      # LAF where the cable enters the top of the well
TMP_MIN <- -20
TMP_MAX <- 120

# lists every xml in the drive folder and parses the timestamp out of each name
build_file_index <- function(folder_id) {
  items <- drive_find(q = sprintf("'%s' in parents", folder_id), corpus = "allDrives")
  items <- items[grepl("\\.xml$", items$name, ignore.case = TRUE), ]
  tibble(name = items$name, id = items$id) %>%
    mutate(
      date_str = str_match(name, "_(\\d{8})_")[, 2],
      time_str = str_match(name, "_\\d{8}_(\\d{6})")[, 2],
      datetime = ymd_hms(paste0(date_str, time_str), tz = "UTC") - hours(3),
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
  col_names <- vapply(curves, function(x) xml_text(xml_find_first(x, ".//mnemonic")), character(1))
  nodes <- xml_find_all(d, "//data")
  if (length(nodes) == 0) return(NULL)
  m <- tryCatch(do.call(rbind, lapply(strsplit(trimws(xml_text(nodes)), ","), as.numeric)),
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
  if (nrow(selected) == 0) stop("no files selected — check your settings")
  tmp <- tempfile(fileext = ".xml")
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  out <- vector("list", nrow(selected))
  for (i in seq_len(nrow(selected))) {
    ok <- tryCatch({ drive_download(as_id(selected$id[i]), path = tmp, overwrite = TRUE); TRUE },
                   error = function(e) FALSE)
    if (ok) out[[i]] <- parse_one_xml(tmp)
    if (i %% 10 == 0 || i == nrow(selected)) message("parsed ", i, " / ", nrow(selected))
  }
  bind_rows(out)
}

# folds one profile into down/up legs on true depth, drops out-of-well junk.
# this is the ONLY place the fold happens — add_depth no longer duplicates it.
fold_profile <- function(df) {
  well_bottom_laf <- TURNAROUND_LAF + (TURNAROUND_LAF - TOP_OF_WELL_LAF)
  df %>%
    filter(TMP > TMP_MIN, TMP < TMP_MAX,
           LAF >= TOP_OF_WELL_LAF, LAF <= well_bottom_laf) %>%
    mutate(
      datetime = ymd_hms(start_time, tz = "UTC") - hours(3),
      label = format(datetime, "%Y-%m-%d %H:%M"),
      leg = if_else(LAF <= TURNAROUND_LAF, "down", "up"),
      depth_m = if_else(LAF <= TURNAROUND_LAF,
                        LAF - TOP_OF_WELL_LAF,
                        2 * TURNAROUND_LAF - LAF - TOP_OF_WELL_LAF),
      depth_ft = depth_m * 3.28084
    ) %>%
    filter(depth_m >= 0)
}

# builds the file selection from the settings you set in the analysis block
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
    return(base %>% filter(as_date(datetime) == as_date(window_day),
                           hour >= window_start_hour, hour < window_end_hour) %>%
             arrange(datetime))
  } else if (mode == "n_per_window") {
    # every obs in the hour window, then thin to n_obs evenly-spaced ones
    win <- base %>%
      filter(as_date(datetime) == as_date(window_day),
             hour >= window_start_hour, hour < window_end_hour) %>%
      arrange(datetime)
    if (nrow(win) <= n_obs) return(win)   # fewer than requested — return all
    idx <- round(seq(1, nrow(win), length.out = n_obs))
    return(win[idx, ])
  } else if (mode == "datetime_range") {
    # spans a full start->end datetime (can cross days), keeps obs nearest
    # every `every_hours` from the start
    start_dt <- ymd_hm(range_start_dt, tz = "UTC")
    end_dt   <- ymd_hm(range_end_dt,   tz = "UTC")
    win <- base %>% filter(datetime >= start_dt, datetime <= end_dt) %>% arrange(datetime)
    if (nrow(win) == 0) return(win)
    # target times: start, start+every_hours, start+2*every_hours, ... up to end
    targets <- seq(start_dt, end_dt, by = paste(every_hours, "hours"))
    # for each target, grab the observation closest to it
    picked <- lapply(targets, function(tt) {
      win %>% mutate(gap = abs(as.numeric(difftime(datetime, tt, units = "mins")))) %>%
        slice_min(gap, n = 1, with_ties = FALSE)
    })
    return(bind_rows(picked) %>% distinct(name, .keep_all = TRUE) %>% select(-gap))
  } else stop("mode must be day_of_month, date_range, single_day, time_window, or n_per_window")
  base %>%
    mutate(date_only = as_date(datetime),
           mins_from_target = abs((hour * 60 + minute) - target_hour * 60)) %>%
    group_by(date_only) %>% slice_min(mins_from_target, n = 1, with_ties = FALSE) %>%
    ungroup() %>% select(-date_only, -mins_from_target)
}

# --- the slow one-time work ---
drive_auth()
index <- build_file_index(FOLDER_ID)
cat("indexed", nrow(index), "files, spanning",
    format(min(index$datetime, na.rm = TRUE)), "to",
    format(max(index$datetime, na.rm = TRUE)), "\n")

############################################################
#  ANALYSIS — highlight and run this whole block to make plots.
#  Edit the settings, run it, get plots. Re-run with different settings anytime.
############################################################

# --- settings ---
target_hour <- 12            # hour (CST) to sample near, for the thinning modes
SHOW_UP_LEG <- FALSE         # TRUE to also draw the up-leg

mode <- "datetime_range"       # "day_of_month", "date_range", "single_day", "time_window", "n_per_window"
                           # "datetime_range"


range_start_dt <- "2026-07-06 08:00"   # "YYYY-MM-DD HH:MM" (CST)
range_end_dt   <- "2026-07-08 02:00"
every_hours    <- 6                     # one observation every 6 hours

window_day  <- "2026-07-08"; window_start_hour <- 0; window_end_hour <- 24
n_obs <- 12                   # how many evenly-spaced observations to show in the window

window_day  <- "2026-07-08"; window_start_hour <- 0; window_end_hour <- 24
day_of_month <- 15
range_start <- "2026-07-01"; range_end <- "2026-07-05"
single_day  <- "2026-07-07"

# --- fetch, fold ---
picks <- pick_files()
cat("selected", nrow(picks), "files\n")

folded <- fetch_and_parse(picks) %>% fold_profile()

# --- plot ---
plot_data <- folded %>% filter(SHOW_UP_LEG | leg == "down")

ggplot(plot_data, aes(TMP, depth_ft, color = label, linetype = leg,
                      group = interaction(start_time, leg))) +
  geom_path(linewidth = 0.6) +
  scale_y_reverse() +
  coord_cartesian(xlim = c(10, 12)) +
  scale_linetype_manual(values = c(down = "solid", up = "dashed"), guide = "none") +
  scale_color_viridis_d(option = "viridis") +
  labs(x = "Temperature (°C)", y = "Depth below well top (ft)",
       color = "observation (CST)",
       title = "Darcy Observation Well — temperature vs depth") +
  theme_bw()