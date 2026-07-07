###########################
#  WHAT THIS DOES:
#  Reads every .xml file in a Google Drive folder (one at a time, so nothing
#  piles up on your disk), parses out the data table + optical channels
#  (LAF, ST, AST, REV-ST, REV-AST, TMP), stamps each row with channel and
#  timestamp, and appends the rows to a per-channel CSV on your computer.
#
#  HOW TO RUN:
#   1. Set FOLDER_ID (the Google Drive folder) and OUTPUT_DIR below.
#   2. Source the whole script. Walk away. Come back to CSVs.
#   3. If it stops partway, just source it again; it picks up where it left off.
###########################

#### 1 permission to grant R access to google drive ####
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



library(googledrive)

#need to run this to give R access to your drive
#type 1 in terminal to grant access 
#go to browser and click continue
drive_auth()   
# shared <- drive_find(q = "sharedWithMe = true")
# print(shared, n = 50)


folder_id <- drive_get(as_id("1wQGImxaQGcEA_156JJBwIDPLrG86JfZQ"))

# files <- drive_find(
#   q = "'1wQGImxaQGcEA_156JJBwIDPLrG86JfZQ' in parents",
#   corpus = "allDrives"
# )
# nrow(files)
# files


## TAKES A LONG TIME - builds the queryable filename index once per session
index <- build_file_index(folder_id)

# pick the 15th of every month, channel 1, nearest noon
picks <- index %>%
  mutate(date_only = as_date(datetime),
         mins_from_target = abs((hour * 60 + minute) - 12 * 60)) %>%
  group_by(date_only) %>%
  slice_min(mins_from_target, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-date_only, -mins_from_target)

nrow(picks)   # how many months you got




# The Google Drive folder ID (the long string from drive_get / the URL).
FOLDER_ID  <- "1wQGImxaQGcEA_156JJBwIDPLrG86JfZQ"

#where the finished CSVs + the progress log get written (a LOCAL folder).
OUTPUT_DIR <- "~/GitHub/summer_26/darcy_well/csv_output"

#how often to print a progress message 
PROGRESS_EVERY <- 50

library(googledrive)
library(xml2)

OUTPUT_DIR <- path.expand(OUTPUT_DIR)
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# The progress log
DONE_LOG <- file.path(OUTPUT_DIR, "_processed_ids.txt")
done_ids <- if (file.exists(DONE_LOG)) readLines(DONE_LOG) else character(0)


#the Parser

parse_one_xml <- function(path) {
  d <- tryCatch(xml_ns_strip(read_xml(path)), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  
  # channel number, dug out of "...channel:0 double ended."
  logname <- xml_text(xml_find_first(d, "//log/name"))
  channel <- sub(".*channel:\\s*([0-9]+).*", "\\1", logname)
  if (is.na(channel) || channel == logname) channel <- "unknown"
  
  # timestamps (kept as text — unambiguous ISO format)
  t_start <- xml_text(xml_find_first(d, "//startDateTimeIndex"))
  t_end   <- xml_text(xml_find_first(d, "//endDateTimeIndex"))
  
  # column names, read from THIS file's header (auto-adapts if curves differ)
  curves <- xml_find_all(d, "//logCurveInfo")
  col_names <- vapply(curves,
                      function(x) xml_text(xml_find_first(x, ".//mnemonic")),
                      character(1))
  
  # the numeric rows, e.g. "-216.334,0.0105,...,223.723"
  data_nodes <- xml_find_all(d, "//data")
  if (length(data_nodes) == 0) return(NULL)
  split_rows <- strsplit(trimws(xml_text(data_nodes)), ",")
  
  m <- tryCatch(do.call(rbind, lapply(split_rows, as.numeric)),
                warning = function(w) NULL, error = function(e) NULL)
  if (is.null(m)) return(NULL)
  
  if (ncol(m) != length(col_names)) col_names <- paste0("V", seq_len(ncol(m)))
  
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- col_names
  df$channel     <- channel
  df$start_time  <- t_start
  df$end_time    <- t_end
  df
}


#appends rows to the correct csv

append_to_channel_csv <- function(df, source_name) {
  df$source_file <- source_name
  ch <- df$channel[1]
  out_path <- file.path(OUTPUT_DIR, paste0("channel_", ch, ".csv"))
  write.table(df, out_path,
              sep = ",", row.names = FALSE,
              col.names = !file.exists(out_path),  # header only if new file
              append = file.exists(out_path),
              qmethod = "double")
}


#lists the files in drive folder

message("Listing files in Drive folder...")
all_items <- drive_find(
  q = sprintf("'%s' in parents", FOLDER_ID),
  corpus = "allDrives"
)

# keep only .xml (in case there are stray non-xml items)
xml_items <- all_items[grepl("\\.xml$", all_items$name, ignore.case = TRUE), ]
message("Found ", nrow(xml_items), " .xml files.")

if (nrow(xml_items) == 0) stop("No .xml files found in that folder.")


# stream, parse, append
tmp <- tempfile(fileext = ".xml")   # one reusable download slot
n   <- nrow(xml_items)
skipped <- 0
failed  <- character(0)

for (i in seq_len(n)) {
  
  file_id   <- xml_items$id[i]
  file_name <- xml_items$name[i]
  
  #skip files we already processed in a previous run
  if (file_id %in% done_ids) { skipped <- skipped + 1; next }
  
  # download this one file (overwrite the temp slot each time)
  ok <- tryCatch({
    drive_download(as_id(file_id), path = tmp, overwrite = TRUE, verbose = FALSE)
    TRUE
  }, error = function(e) FALSE)
  
  if (!ok) { failed <- c(failed, file_name); next }
  
  # parse + append
  df <- parse_one_xml(tmp)
  if (is.null(df)) {
    failed <- c(failed, file_name)
  } else {
    append_to_channel_csv(df, file_name)
    # mark done (append to log immediately so a crash doesn't lose it)
    cat(file_id, "\n", file = DONE_LOG, append = TRUE)
  }
  
  if (i %% PROGRESS_EVERY == 0 || i == n) {
    message("  ", i, " / ", n,
            "  (skipped already-done: ", skipped,
            ", failed: ", length(failed), ")")
  }
}

if (file.exists(tmp)) file.remove(tmp)


#SUMMARY

message("\nDONE.")
message("CSVs written to: ", OUTPUT_DIR)
csvs <- list.files(OUTPUT_DIR, pattern = "^channel_.*\\.csv$", full.names = TRUE)
for (f in csvs) {
  nlines <- length(count.fields(f, sep = ",")) - 1  # minus header
  message("  ", basename(f), ": ~", nlines, " data rows")
}
if (length(failed) > 0) {
  message("\n", length(failed), " files failed/skipped as unreadable. First few:")
  message("  ", paste(head(failed, 5), collapse = "\n  "))
}

