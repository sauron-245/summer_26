

library(googledrive)
library(xml2)
library(dplyr)
library(tibble)
library(lubridate)
library(stringr)
library(DBI)
library(RPostgres)
library(ggplot2)


# connects to the dts postgres database (read-only)
connect_dts <- function() {
  dbConnect(RPostgres::Postgres(),
            dbname = "dts_db", host = "dts.physics.carleton.edu",
            port = "5432", user = "dts_user_ro",
            password = "$$N0th1ng5p3c14l$$")
}

# pulls the laf -> depth pairs from the database for each channel, since the
# cable geometry is fixed these older mappings apply to the new xml too
get_depth_maps <- function() {
  con <- connect_dts()
  on.exit(dbDisconnect(con), add = TRUE)
  
  q <- "
    SELECT H.channel_name, D.laf_m, D.depth_m
    FROM dts_data D
    INNER JOIN measurement M ON M.id = D.measurement_id
    INNER JOIN channel H ON M.channel_id = H.id
    WHERE D.depth_m IS NOT NULL
      AND M.id IN (
        SELECT MIN(id) FROM measurement GROUP BY channel_id
      )
    ORDER BY H.channel_name, D.laf_m;"
  
  dbGetQuery(con, q) %>% as_tibble()
}

# turns laf values into depth by interpolating along that channel's mapping
laf_to_depth <- function(laf, channel_name, maps) {
  m <- maps[[channel_name]]
  if (is.null(m)) return(rep(NA_real_, length(laf)))
  approx(x = m$laf_m, y = m$depth_m, xout = laf, rule = 2)$y
}


# lists every xml in the drive folder and parses the timestamp out of each name
build_file_index <- function(folder_id) {
  items <- drive_find(q = sprintf("'%s' in parents", folder_id),
                      corpus = "allDrives")
  items <- items[grepl("\\.xml$", items$name, ignore.case = TRUE), ]
  
  tibble(name = items$name, id = items$id) %>%
    mutate(
      file.path.length = nchar(name),
      channel  = str_match(name, "channel\\s*([0-9]+)")[, 2],
      date_str = str_match(name, "_(\\d{8})_")[, 2],
      time_str = str_match(name, "_\\d{8}_(\\d{6})")[, 2],
      datetime = ymd_hms(paste0(date_str, time_str), tz = "UTC"),
      year = year(datetime), month = month(datetime), day = day(datetime),
      hour = hour(datetime), minute = minute(datetime)
    )
}

# filters the file index down to the dates, channels, and sampling the user wants
select_files <- function(index, start_date, end_date,
                         channels = NULL, one_per_day = FALSE, target_hour = 12) {
  sel <- index %>%
    filter(as_date(datetime) >= as_date(start_date),
           as_date(datetime) <= as_date(end_date))
  
  if (!is.null(channels)) sel <- sel %>% filter(channel %in% channels)
  
  if (one_per_day) {
    sel <- sel %>%
      mutate(date_only = as_date(datetime),
             mins_from_target = abs((hour * 60 + minute) - target_hour * 60)) %>%
      group_by(channel, date_only) %>%
      slice_min(mins_from_target, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(-date_only, -mins_from_target)
  }
  sel
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

# downloads each selected file one at a time, parses it, and tags it with the
# channel from the filename (the internal channel numbering is off by one)
fetch_and_parse <- function(selected) {
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

# drops unlabeled points and bad temps, then converts laf to real depth using
# the matching channel's database mapping
add_depth <- function(df, channel_maps) {
  df %>%
    filter(!is.na(borehole), TMP > -20, TMP < 120) %>%
    mutate(channel_name = paste("channel", channel)) %>%
    group_by(channel_name) %>%
    mutate(depth_m = laf_to_depth(LAF, channel_name[1], channel_maps)) %>%
    ungroup()
}


# run it 

folder_id <- "12tkE_ITIb1XxKthTA4BVB965zs5w4sTe"
drive_auth()

# grabs the depth lookups once and splits them by channel for fast interpolation
depth_raw <- get_depth_maps()
channel_maps <- split(depth_raw, depth_raw$channel_name)

## TAKES A LONG TIME - builds the queryable filename index once per session
index <- build_file_index(folder_id)

#######################################################
#  USER SETTINGS — edit these to choose what data you want
#######################################################


which_channel <- "1"          # keep at 1 for now (3 is in another folder)
target_hour   <- 20           # hour of day (UTC, 0-23) to sample near, e.g. 12 = noon

# how to choose which days: pick ONE mode by setting it to TRUE (set other to FALSE)
mode_day_of_month <- FALSE     # one observation per month, on a chosen day of the month
mode_date_range   <- TRUE    # every day between two dates

# settings for mode_day_of_month:
day_of_month <- 15            # e.g. 15 = the 15th of each month

# settings for mode_date_range:
range_start <- "2025-10-01"   # "YYYY-MM-DD"
range_end   <- "2026-10-31"

#######################################################
#  (you shouldn't need to edit below here)
#######################################################


# builds the file selection from your settings above
pick_files <- function() {
  base <- index %>% filter(channel == which_channel)
  
  if (mode_day_of_month) {
    base <- base %>% filter(day == day_of_month)
  } else if (mode_date_range) {
    base <- base %>%
      filter(as_date(datetime) >= as_date(range_start),
             as_date(datetime) <= as_date(range_end))
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

# quick check - what did you get?
cat("selected", nrow(picks), "files\n")
picks %>% distinct(as_date(datetime)) %>% arrange(`as_date(datetime)`) %>% print(n = 50)

# fetch, label, add depth
dat <- fetch_and_parse(picks) %>% add_boreholes() %>% add_depth(channel_maps)

# plots temperature against real depth for s1, one line per measurement time
dat %>%
  filter(borehole == "S1") %>%
  mutate(month_label = format(datetime, "%Y-%m")) %>%
  ggplot(aes(TMP, depth_m, group = start_time, color = month_label)) +
  geom_path(linewidth = 0.5) +
  scale_y_reverse() +
  xlim(0, 25) +
  scale_color_viridis_d(option = "viridis") +
  labs(x = "Temperature (°C)", y = "Depth (m)", color = "month",
       title = "15th of Every Month Nov 2025-Mar 2026") +
  theme_bw()

nrow(picks)
range(index$datetime, na.rm = TRUE)
index %>% filter(channel == "1") %>% count(as_date(datetime)) %>% head(20)



# connects to the dts postgres database (read-only)
connect_dts <- function() {
  dbConnect(RPostgres::Postgres(),
            dbname = "dts_db", host = "dts.physics.carleton.edu",
            port = "5432", user = "dts_user_ro",
            password = "$$N0th1ng5p3c14l$$")
}


# =====================================================================
#  PART 1 — database: 15th of every month in 2022 and 2023, S1 only
# =====================================================================

# pulls channel-1 baldspot points on the 15th of each month across 2022-2023,
# keeping real depth straight from the database
get_db_15ths <- function() {
  con <- connect_dts()
  on.exit(dbDisconnect(con), add = TRUE)
  
  q <- "
    SELECT M.id AS measurement_id, M.datetime_utc,
           D.laf_m, D.depth_m, D.temperature_c
    FROM dts_data D
    INNER JOIN measurement M ON M.id = D.measurement_id
    INNER JOIN channel H ON M.channel_id = H.id
    WHERE H.channel_name = 'channel 1'
      AND H.fiber_topology_name = 'baldspot'
      AND date_part('day', M.datetime_utc) = 15
      AND date_part('year', M.datetime_utc) IN (2019, 2020, 2021, 2022, 2023)
    ORDER BY M.datetime_utc, D.laf_m;"
  
  dbGetQuery(con, q) %>% as_tibble()
}

# keeps the measurement nearest noon each day, clips to S1, standardizes columns
prep_db <- function(df, target_hour = 12) {
  df %>%
    mutate(datetime = as_datetime(datetime_utc, tz = "UTC"),
           date_only = as_date(datetime),
           hour = hour(datetime), minute = minute(datetime),
           mins_from_target = abs((hour * 60 + minute) - target_hour * 60)) %>%
    group_by(date_only) %>%
    filter(measurement_id == measurement_id[which.min(mins_from_target)]) %>%
    ungroup() %>%
    filter(laf_m > 230.3, laf_m < 388.8) %>%          # S1 window
    transmute(datetime,
              depth_m,
              temperature_c,
              obs_id = format(datetime, "%Y-%m-%d %H:%M"),
              month_label = format(datetime, "%Y-%m"),
              source = "database")
}


# =====================================================================
#  PART 2 — your 2025 XML result (already in `dat` from your workflow)
# =====================================================================
#  Run your Google Drive workflow first so `dat` exists with S1 labeled and
#  depth_m added. This standardizes it to the same columns as the database side.

prep_xml <- function(dat) {
  dat %>%
    filter(borehole == "S1") %>%
    transmute(datetime,
              depth_m,
              temperature_c = TMP,
              obs_id = format(datetime, "%Y-%m-%d %H:%M"),
              month_label = format(datetime, "%Y-%m"),
              source = "xml (2025)")
}


# =====================================================================
#  PART 3 — combine and plot
# =====================================================================

db_s1  <- get_db_15ths() %>% prep_db()
xml_s1 <- prep_xml(dat)          # dat must already exist from your XML workflow

combined <- bind_rows(db_s1, xml_s1)



plot_df <- combined %>%
  mutate(
    month = factor(lubridate::month(datetime)),   # 1-12, color
    linetype_src = if_else(source == "xml (2025)", "xml", "database")
  )


ggplot(plot_df, aes(temperature_c, depth_m,
                    group = obs_id, color = month)) +
  geom_path(aes(linewidth = linetype_src, alpha = linetype_src)) +
  scale_y_reverse() +
  coord_cartesian(xlim = c(5, 20)) +
  scale_color_viridis_d(option = "viridis") +
  scale_linewidth_manual(values = c(database = 0.4, xml = .4)) +
  scale_alpha_manual(values = c(database = .9, xml = .35)) +
  labs(x = "Temperature (°C)", y = "Depth (m)",
       color = "month", linewidth = "source", alpha = "source",
       title = "Bald Spot S1 — 15th of each month, colored by month") +
  theme_bw()




# --- database: mean daily temp per observation, S1, all available dates ------
# (broaden the earlier query to all years, not just 2022-2023, to match Fig 3)
get_db_alltime <- function() {
  con <- connect_dts()
  on.exit(dbDisconnect(con), add = TRUE)
  q <- "
    SELECT M.datetime_utc, D.laf_m, D.temperature_c
    FROM dts_data D
    INNER JOIN measurement M ON M.id = D.measurement_id
    INNER JOIN channel H ON M.channel_id = H.id
    WHERE H.channel_name = 'channel 1'
      AND H.fiber_topology_name = 'baldspot'
      AND D.laf_m > 230.3 AND D.laf_m < 388.8   -- S1 window
    ORDER BY M.datetime_utc;"
  dbGetQuery(con, q) %>% as_tibble()
}

db_daily <- get_db_alltime() %>%
  mutate(date = as_date(datetime_utc)) %>%
  group_by(date) %>%
  summarize(mean_temp = mean(temperature_c, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "database")

# --- xml: same reduction from your `dat` (S1, raw TMP) -----------------------
xml_daily <- dat %>%
  filter(borehole == "S1", TMP > -20, TMP < 120) %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarize(mean_temp = mean(TMP, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "xml 2025")

# --- combine and plot --------------------------------------------------------
daily_all <- daily_all %>%


ggplot(daily_all, aes(date, mean_temp, color = source)) +
  geom_line(data = filter(daily_all, source == "database"),
            linewidth = 0.4) +
  geom_point(data = filter(daily_all, source != "database"),
             size = 1.0, shape = 17) +
  scale_color_manual(values = c("database" = "darkgrey",
                                "xml 2025" = "red")) +
  labs(x = "Year", y = "Mean borehole temperature (°C)",
       color = NULL,
       title = "Bald Spot S1 — mean daily temperature over time plus extra")
  theme_bw() +
  theme(legend.position = "bottom")


