# =============================================================================
#  Bald Spot DTS — query XML on Google Drive by date/time, parse, clip, plot
# =============================================================================
#  Implements the workflow:
#   1. Read the Drive folder -> tibble of filenames
#   2. Parse date/time OUT OF the filenames -> queryable columns
#   3. User picks dates/times -> filter the tibble (cheap, no downloading)
#   4. Download + parse ONLY the selected files
#   5. Clip by LAF to label boreholes (both channels)
#   6. Ready for plotting
#
#  The efficiency trick: steps 1-3 use only filenames (fast). Only step 4
#  touches file contents, and only for the few files you actually asked for.
# =============================================================================

library(googledrive)
library(xml2)
library(dplyr)
library(tibble)
library(lubridate)
library(stringr)


# ===== STEP 1 + 2: BUILD THE QUERYABLE FILENAME TIBBLE =======================
#  Lists every .xml in a Drive folder and parses the timestamp from the name.
#  Returns a tibble with: name, id (Drive file id, needed to download later),
#  channel, datetime, and split date/time parts.

build_file_index <- function(folder_id) {
  
  message("Listing files on Google Drive...")
  items <- drive_find(
    q = sprintf("'%s' in parents", folder_id),
    corpus = "allDrives"
  )
  
  # keep only .xml
  items <- items[grepl("\\.xml$", items$name, ignore.case = TRUE), ]
  message("Found ", nrow(items), " xml files.")
  
  # Parse pieces out of names like:
  #   "channel 1_UTC_20251026_141922.346.xml"
  #    channel ^1      date^      ^time ^ms
  idx <- tibble(
    name = items$name,
    id   = items$id
  ) %>%
    mutate(
      file.path.length = nchar(name),
      channel  = str_match(name, "channel\\s*([0-9]+)")[, 2],
      date_str = str_match(name, "_(\\d{8})_")[, 2],          # YYYYMMDD
      time_str = str_match(name, "_\\d{8}_(\\d{6})")[, 2],    # HHMMSS
      datetime = ymd_hms(paste0(date_str, time_str), tz = "UTC"),
      year = year(datetime), month = month(datetime), day = day(datetime),
      hour = hour(datetime), minute = minute(datetime)
    )
  
  idx
}


# ===== STEP 3: FILTER THE INDEX BY WHAT THE USER WANTS ========================
#  Flexible: single day (start==end), a range, optionally one-per-day nearest
#  a target hour. Operates on filenames only — nothing is downloaded here.

select_files <- function(index,
                         start_date, end_date,
                         channels    = NULL,        # e.g. c("1","3"); NULL = all
                         one_per_day = FALSE,
                         target_hour = 12) {
  
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
  
  message("Selected ", nrow(sel), " files to download.")
  sel
}


# ===== STEP 4: DOWNLOAD + PARSE THE SELECTED FILES ===========================
#  Streams each selected file to a temp slot, parses it, discards it.

parse_one_xml <- function(path) {
  d <- tryCatch(xml_ns_strip(read_xml(path)), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  
  logname <- xml_text(xml_find_first(d, "//log/name"))
  channel <- sub(".*channel:\\s*([0-9]+).*", "\\1", logname)
  t_start <- xml_text(xml_find_first(d, "//startDateTimeIndex"))
  
  curves <- xml_find_all(d, "//logCurveInfo")
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
  df$channel    <- channel
  df$start_time <- t_start
  df
}

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
        parsed$channel <- selected$channel[i]   # <-- use FILENAME channel
        out[[i]] <- parsed
      }
    }
    if (i %% 10 == 0 || i == nrow(selected))
      message("  parsed ", i, " / ", nrow(selected))
  }
  bind_rows(out)
}

# ===== STEP 5: CLIP BY LAF TO LABEL BOREHOLES (both channels) ================

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

library(DBI); library(RPostgres); library(dplyr)

# --- pull the laf->depth pairs for S1 from the database (once) ---
con <- dbConnect(RPostgres::Postgres(), dbname="dts_db",
                 host="dts.physics.carleton.edu", port="5432",
                 user="dts_user_ro", password="$$N0th1ng5p3c14l$$")

# grab one representative measurement's laf/depth pairs for channel 1.
# depth_m is the same across measurements (fixed geometry), so one is enough,
# but we pull distinct pairs to be safe.
laf_depth <- dbGetQuery(con, "
  SELECT DISTINCT D.laf_m, D.depth_m
  FROM dts_data D
  INNER JOIN measurement M ON M.id = D.measurement_id
  INNER JOIN channel H ON M.channel_id = H.id
  WHERE H.fiber_topology_name = 'baldspot'
    AND H.channel_name = 'channel 1'
    AND D.depth_m IS NOT NULL
  ORDER BY D.laf_m;")
dbDisconnect(con)

# --- build a converter function via interpolation ---
laf_to_depth <- function(laf) {
  approx(x = laf_depth$laf_m, y = laf_depth$depth_m,
         xout = laf, rule = 2)$y   # rule=2 = clamp at the ends, no NAs
}

# --- apply it to your XML data ---
dat <- dat %>% mutate(depth_m = laf_to_depth(LAF))

# ===== PUTTING IT TOGETHER ===================================================
#  Example end-to-end usage. Build the index ONCE, then query it as many times
#  as you like without re-listing the drive.

FOLDER_ID <- "12tkE_ITIb1XxKthTA4BVB965zs5w4sTe"   # your "channel 1" folder
drive_auth()

# # build index once (cheap; only filenames)
index <- build_file_index(FOLDER_ID)
#
# # peek at it, just like your professor's example:
index %>% select(name, file.path.length)
#
# # query it: one obs/day nearest noon, Oct 2025, channel 1
picks <- select_files(index, "2025-10-26", "2025-10-26",
  channels = "1", one_per_day = TRUE, target_hour = 12)
#
# # download + parse only those
dat <- fetch_and_parse(picks)
#
# # clip to boreholes
dat <- add_boreholes(dat)
#
# plot, same style as your checkups script
library(ggplot2)
dat %>% filter(!is.na(borehole)) %>%
ggplot(aes(TMP, LAF, color = as.factor(hour))) +
  geom_path() + scale_y_reverse() + theme_bw() +
  facet_grid(cols = vars(borehole))


table(dat$borehole, useNA = "always")
table(dat$channel)
dat %>% distinct(channel)        # what parse_one_xml pulled from inside the file
picks %>% distinct(channel)      # what the filename said
nrow(dat)                        # did anything get parsed at all?
range(dat$LAF, na.rm = TRUE)     # are LAF values in the expected range?
