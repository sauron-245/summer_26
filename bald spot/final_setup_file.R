############################################################
#  Bald Spot DTS — SETUP
#
#  Source this ONCE per session. It loads all the functions and does the slow
#  one-time work: google drive auth, the laf->depth lookups, and indexing the
#  ~8000 xml filenames.
#
#  After this, source any of the analysis scripts as many times as you like
#  without redoing any of it.
############################################################

library(googledrive)
library(xml2)
library(dplyr)
library(tibble)
library(lubridate)
library(stringr)
library(DBI)
library(RPostgres)
library(ggplot2)
library(plotly)

# --- fixed settings ---------------------------------------------------------

# the google drive folders holding the xml (one per channel)
FOLDER_IDS <- c("1" = "12tkE_ITIb1XxKthTA4BVB965zs5w4sTe",   # channel 1
                "3" = "1SeUUnrZ0CGNRo88OA_GUusndQCnY-7c5")   # channel 3

# borehole laf windows
BOREHOLE_LAF <- list(
  S1 = c(230.3, 388.8),
  S2 = c(775.1, 933.6),
  S3 = c(1255.9, 1424.4),
  S4 = c(562.0, 730.5),
  S5 = c(123.0, 291.5)
)

TMP_MIN <- -20    # drop physically impossible temperatures
TMP_MAX <- 120


# --- database ---------------------------------------------------------------

# connects to the dts postgres database (read-only)
connect_dts <- function() {
  dbConnect(RPostgres::Postgres(),
            dbname = "dts_db", host = "dts.physics.carleton.edu",
            port = "5432", user = "dts_user_ro",
            password = "$$N0th1ng5p3c14l$$")
}

# pulls the laf -> depth pairs for each channel from one measurement, since the
# cable geometry is fixed these apply to every observation including the xml
get_depth_maps <- function() {
  con <- connect_dts()
  on.exit(dbDisconnect(con), add = TRUE)
  q <- "
    SELECT H.channel_name, D.laf_m, D.depth_m
    FROM dts_data D
    INNER JOIN measurement M ON M.id = D.measurement_id
    INNER JOIN channel H ON M.channel_id = H.id
    WHERE D.depth_m IS NOT NULL
      AND M.id IN (SELECT MIN(id) FROM measurement GROUP BY channel_id)
    ORDER BY H.channel_name, D.laf_m;"
  dbGetQuery(con, q) %>% as_tibble()
}

# turns laf values into depth by interpolating along that channel's mapping
laf_to_depth <- function(laf, channel_name, maps) {
  m <- maps[[channel_name]]
  if (is.null(m)) return(rep(NA_real_, length(laf)))
  approx(x = m$laf_m, y = m$depth_m, xout = laf, rule = 2)$y
}

# generic database pull: any channel, any borehole, any date filter as sql
get_db_data <- function(where_clause, laf_min, laf_max, channel_name = "channel 1") {
  con <- connect_dts()
  on.exit(dbDisconnect(con), add = TRUE)
  q <- sprintf("
    SELECT M.id AS measurement_id, M.datetime_utc,
           D.laf_m, D.depth_m, D.temperature_c
    FROM dts_data D
    INNER JOIN measurement M ON M.id = D.measurement_id
    INNER JOIN channel H ON M.channel_id = H.id
    WHERE H.channel_name = '%s'
      AND H.fiber_topology_name = 'baldspot'
      AND D.laf_m > %f AND D.laf_m < %f
      AND %s
    ORDER BY M.datetime_utc, D.laf_m;",
               channel_name, laf_min, laf_max, where_clause)
  dbGetQuery(con, q) %>% as_tibble()
}

# keeps the observation nearest a target hour on each day, standardizes columns
prep_db <- function(df, target_hour = 12) {
  df %>%
    mutate(datetime = as_datetime(datetime_utc, tz = "UTC"),
           date_only = as_date(datetime),
           hour = hour(datetime), minute = minute(datetime),
           mins_from_target = abs((hour * 60 + minute) - target_hour * 60)) %>%
    group_by(date_only) %>%
    filter(measurement_id == measurement_id[which.min(mins_from_target)]) %>%
    ungroup() %>%
    transmute(datetime, depth_m, temperature_c,
              obs_id = format(datetime, "%Y-%m-%d %H:%M"),
              source = "database")
}


# --- google drive xml -------------------------------------------------------

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

# downloads each selected file one at a time, parses it, tags it with the
# channel from the filename (internal channel numbering is off by one)
fetch_and_parse <- function(selected) {
  if (nrow(selected) == 0) stop("no files selected — check your settings")
  tmp <- tempfile(fileext = ".xml")
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  out <- vector("list", nrow(selected))
  for (i in seq_len(nrow(selected))) {
    ok <- tryCatch({
      drive_download(as_id(selected$id[i]), path = tmp, overwrite = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      parsed <- parse_one_xml(tmp)
      if (!is.null(parsed)) {
        parsed$channel <- selected$channel[i]
        out[[i]] <- parsed
      }
    }
    if (i %% 10 == 0 || i == nrow(selected))
      message("parsed ", i, " / ", nrow(selected))
  }
  bind_rows(out)
}

# labels each point with its borehole based on channel and laf window
add_boreholes <- function(df) {
  df %>%
    mutate(borehole = case_when(
      channel == "1" & LAF > 230.3  & LAF < 388.8  ~ "S1",
      channel == "1" & LAF > 775.1  & LAF < 933.6  ~ "S2",
      channel == "1" & LAF > 1255.9 & LAF < 1424.4 ~ "S3",
      channel == "3" & LAF > 562.0  & LAF < 730.5  ~ "S4",
      channel == "3" & LAF > 123.0  & LAF < 291.5  ~ "S5",
      TRUE ~ NA_character_
    ),
    datetime = ymd_hms(start_time, tz = "UTC"),
    hour = hour(datetime))
}

# drops unlabeled points and bad temps, then converts laf to depth
add_depth <- function(df, maps) {
  df %>%
    filter(!is.na(borehole), TMP > TMP_MIN, TMP < TMP_MAX) %>%
    mutate(channel_name = paste("channel", channel)) %>%
    group_by(channel_name) %>%
    mutate(depth_m = laf_to_depth(LAF, channel_name[1], maps)) %>%
    ungroup()
}

# standardizes xml output to the same columns the database side produces
prep_xml <- function(df, bh) {
  df %>%
    filter(borehole == bh) %>%
    transmute(datetime, depth_m, temperature_c = TMP,
              obs_id = format(datetime, "%Y-%m-%d %H:%M"),
              source = "xml")
}

# one helper the analysis scripts use: pick xml files from the index by a
# filter expression, then keep the obs nearest target_hour each day
pick_xml <- function(filter_expr, target_hour = 12, channel_id = "1") {
  index %>%
    filter(channel == channel_id) %>%
    filter({{ filter_expr }}) %>%
    mutate(date_only = as_date(datetime),
           mins_from_target = abs((hour * 60 + minute) - target_hour * 60)) %>%
    group_by(date_only) %>%
    slice_min(mins_from_target, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-date_only, -mins_from_target)
}


############################################################
#  THE SLOW PART — runs once when you source this file
############################################################

drive_auth()

message("pulling depth maps...")
depth_raw <- get_depth_maps()
channel_maps <- split(depth_raw, depth_raw$channel_name)

message("indexing drive files (slow)...")
index <- bind_rows(lapply(names(FOLDER_IDS), function(ch) {
  message("  indexing channel ", ch, "...")
  build_file_index(FOLDER_IDS[[ch]])
}))

cat("\nSETUP DONE\n")
cat("  indexed", nrow(index), "xml files spanning",
    format(min(index$datetime, na.rm = TRUE)), "to",
    format(max(index$datetime, na.rm = TRUE)), "\n")
print(index %>% count(channel))

#############################################################
#  Pinch-point analysis functions.
#
#  Background (Fandel et al. 2025): pinch points are depth intervals where the
#  borehole temperature stays near the pre-operational baseline even while the
#  rest of the profile swings hot/cold with the seasons. They mark intervals of
#  high horizontal groundwater flow along fractures — flowing water near the
#  mean annual temperature keeps flushing the thermal anomaly away. So the
#  defining property is LOW SEASONAL AMPLITUDE, not a particular temperature.
#  Non-pinch intervals have poor flow, so heat accumulates and they swing far
#  from baseline.
############################################################


# the pre-operational baseline temperature (deep stable ground temp, ~10 °C).
# used as the reference that pinch points stay close to.
BASELINE_TEMP <- 10.0

# named depth intervals you care about. edit/extend freely.
# these are examples — set them from your own reading of the profiles.
DEPTH_INTERVALS <- list(
  pinch     = c(30, 40),     # a depth range you judge to be a pinch point
  non_pinch = c(60, 70)      # a depth range you judge NOT to be a pinch point
)


# pulls out the rows falling inside a depth interval and tags them with its name
extract_interval <- function(df, interval_name, intervals = DEPTH_INTERVALS) {
  rng <- intervals[[interval_name]]
  if (is.null(rng)) stop("no interval named '", interval_name, "'")
  df %>%
    filter(depth_m >= rng[1], depth_m <= rng[2]) %>%
    mutate(interval = interval_name)
}

# pulls every named interval at once and stacks them
extract_all_intervals <- function(df, intervals = DEPTH_INTERVALS) {
  bind_rows(lapply(names(intervals),
                   function(nm) extract_interval(df, nm, intervals)))
}

# reduces each observation x interval to one row of pinch-point metrics.
#   mean_temp  — average temperature in that interval
#   dev        — how far the interval sits from baseline (the pinch metric:
#                small = pinned near baseline = pinch-like)
#   spread     — spread of temperature within the interval at that moment
interval_metrics <- function(df) {
  df %>%
    group_by(obs_id, datetime, interval, source) %>%
    summarize(mean_temp = mean(temperature_c, na.rm = TRUE),
              spread    = max(temperature_c, na.rm = TRUE) -
                min(temperature_c, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(dev = abs(mean_temp - BASELINE_TEMP),
           year = year(datetime),
           month = month(datetime))
}

# the paper's actual pinch metric: peak-to-trough amplitude over a time window.
# a true pinch point has SMALL amplitude (groundwater pins it to baseline);
# a non-pinch interval swings widely. computed per interval per year.
interval_amplitude <- function(metrics_df, by = "year") {
  metrics_df %>%
    group_by(interval, source, across(all_of(by))) %>%
    summarize(amplitude = max(mean_temp, na.rm = TRUE) -
                min(mean_temp, na.rm = TRUE),
              n_obs = n(),
              mean_dev = mean(dev, na.rm = TRUE),
              .groups = "drop")
}

