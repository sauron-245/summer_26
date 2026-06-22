
setwd("~/Desktop/summer_26/bald spot/sample_xml")
getwd()

list.files(pattern = "\\.xml$")

library(xml2)
d <- read_xml("channel 1_UTC_20251026_141922.346.xml")
substr(as.character(d), 1, 4000)

substr(as.character(d), 1, 10000)

nchar(as.character(d))

library(xml2)
d2 <- xml_ns_strip(d)
curves <- xml_find_all(d2, "//logCurveInfo")
sapply(curves, function(x) xml_text(xml_find_first(x, ".//mnemonic")))

first_row <- xml_text(xml_find_first(d2, "//data"))
length(strsplit(trimws(first_row), ",")[[1]])


# =============================================================================
#  Bald Spot DTS  —  XML  ->  one CSV per channel
# =============================================================================
#  WHAT THIS DOES (plain English):
#  Looks in a folder full of WITSML .xml files (one measurement each).
#  Opens every file, pulls out the data table inside, stamps each row with
#  the channel number and the timestamp from that file, and stacks them all
#  up. At the end it writes ONE csv per channel (channel_0.csv, channel_1.csv,
#  etc.) into an output folder.
#
#  HOW TO USE:
#  1. Set INPUT_DIR below to the folder that has your .xml files.
#  2. Set OUTPUT_DIR to where you want the finished CSVs to land.
#  3. Hit "Source" in RStudio (top-right of the editor), or run it line by line.
#  4. Watch the messages in the console. Walk away. Come back to CSVs.
# =============================================================================


# ---- 0. SETTINGS YOU EDIT ---------------------------------------------------

# Folder containing your .xml files. The "~" means your home folder.
# Example matches where your samples were:
INPUT_DIR  <- "~/Desktop/summer_26/bald spot/sample_xml"

# Folder where the finished CSVs get written. It will be created if missing.
OUTPUT_DIR <- "~/Desktop/summer_26/bald spot/csv_output"

# The column names, IN ORDER, as they appear in each <data> row.
# We confirmed this from your files. If a future file has different curves,
# the script auto-reads them from the file header instead (see below), so
# this is just a fallback / sanity reference.
EXPECTED_COLS <- c("LAF", "ST", "AST", "REV-ST", "REV-AST", "TMP")


# ---- 1. SETUP ---------------------------------------------------------------

# xml2 is the package that reads XML. Install once if you don't have it.
if (!requireNamespace("xml2", quietly = TRUE)) {
  install.packages("xml2")
}
library(xml2)

# Make the output folder if it doesn't exist yet.
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}


# ---- 2. THE FUNCTION THAT READS ONE FILE ------------------------------------
# Given one .xml file path, returns a data.frame: one row per fiber point,
# with the data columns plus channel / datetime / source_file stamped on.
# If anything goes wrong with a file, it returns NULL (and we skip it) so one
# bad file doesn't kill the whole overnight run.

parse_one_xml <- function(path) {
  
  # read_xml opens the file. xml_ns_strip removes the "namespace" gunk
  # (the xmlns="http://www.witsml.org/..." stuff) so our searches are simple.
  d <- tryCatch(xml_ns_strip(read_xml(path)),
                error = function(e) NULL)
  if (is.null(d)) {
    warning("Could not read XML: ", basename(path))
    return(NULL)
  }
  
  # --- channel number ---
  # It's buried in the <name> text, e.g. "...channel:0 double ended."
  # We grab the digits right after "channel:".
  logname <- xml_text(xml_find_first(d, "//log/name"))
  channel <- sub(".*channel:\\s*([0-9]+).*", "\\1", logname)
  # If that pattern didn't match, channel will just equal the whole name;
  # mark it unknown so it's obvious rather than silently wrong.
  if (channel == logname) channel <- "unknown"
  
  # --- timestamp ---
  # startDateTimeIndex is the time the measurement began, e.g.
  # 2025-10-26T14:19:22.346Z . We keep it as-is (text), which is unambiguous.
  t_start <- xml_text(xml_find_first(d, "//startDateTimeIndex"))
  t_end   <- xml_text(xml_find_first(d, "//endDateTimeIndex"))
  
  # --- column names, read straight from THIS file's header ---
  # We pull the <mnemonic> out of each <logCurveInfo>. This means if some
  # files have a different set of curves, we still label them correctly.
  curves <- xml_find_all(d, "//logCurveInfo")
  col_names <- vapply(curves,
                      function(x) xml_text(xml_find_first(x, ".//mnemonic")),
                      character(1))
  
  # --- the actual numbers ---
  # Each <data> tag holds one comma-separated row, e.g.
  # "-216.334,0.0105006,-0.0574207,0.213524,-0.127084,223.723"
  data_nodes <- xml_find_all(d, "//data")
  if (length(data_nodes) == 0) {
    warning("No <data> rows in: ", basename(path))
    return(NULL)
  }
  raw <- trimws(xml_text(data_nodes))               # one string per row
  split_rows <- strsplit(raw, ",")                  # split each on commas
  
  # Turn the list of split strings into a numeric matrix.
  m <- tryCatch(
    do.call(rbind, lapply(split_rows, as.numeric)),
    warning = function(w) NULL,   # non-numeric junk -> bail on this file
    error   = function(e) NULL
  )
  if (is.null(m)) {
    warning("Non-numeric data in: ", basename(path))
    return(NULL)
  }
  
  # Safety check: do the number of columns of data match the number of names?
  if (ncol(m) != length(col_names)) {
    warning("Column count mismatch in ", basename(path),
            " (", ncol(m), " values vs ", length(col_names), " names)")
    # Fall back to generic names so we still capture the data.
    col_names <- paste0("V", seq_len(ncol(m)))
  }
  
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- col_names
  
  # Stamp every row with where/when it came from.
  df$channel     <- channel
  df$start_time  <- t_start
  df$end_time    <- t_end
  df$source_file <- basename(path)
  
  df
}


# ---- 3. FIND ALL THE FILES --------------------------------------------------

xml_files <- list.files(INPUT_DIR, pattern = "\\.xml$",
                        full.names = TRUE, ignore.case = TRUE)

message("Found ", length(xml_files), " XML files in:\n  ", INPUT_DIR)
if (length(xml_files) == 0) {
  stop("No .xml files found. Check that INPUT_DIR is correct.")
}


# ---- 4. LOOP OVER EVERY FILE ------------------------------------------------
# We collect each file's data.frame into a list, then combine per channel.
# A counter + occasional message lets you see progress during a long run.

all_rows <- vector("list", length(xml_files))

for (i in seq_along(xml_files)) {
  all_rows[[i]] <- parse_one_xml(xml_files[i])
  
  # ping every 25 files (and on the very last one)
  if (i %% 25 == 0 || i == length(xml_files)) {
    message("  processed ", i, " / ", length(xml_files), " files")
  }
}

#drop any files that failed (returned NULL).
all_rows <- all_rows[!vapply(all_rows, is.null, logical(1))]
message("Successfully parsed ", length(all_rows), " files.")

#put everything into one big table.
big <- do.call(rbind, all_rows)
message("Total rows across all files: ", nrow(big))


#one channel per csv
channels <- sort(unique(big$channel))
message("Channels found: ", paste(channels, collapse = ", "))

for (ch in channels) {
  subset_ch <- big[big$channel == ch, ]
  out_path  <- file.path(OUTPUT_DIR, paste0("channel_", ch, ".csv"))
  write.csv(subset_ch, out_path, row.names = FALSE)
  message("  wrote ", nrow(subset_ch), " rows -> ", out_path)
}

message("DONE. CSVs are in: ", OUTPUT_DIR)


#########################
#plots
########################

library(dplyr)
library(ggplot2)
library(lubridate)


CSV_PATH <- "~/Desktop/summer_26/bald spot/csv_output/channel_0.csv"

#borehole LAF ranges
#channel 1 (== channel "0" here) boreholes:
#   S1: 230.3 - 388.8 , S2: 775.1 - 933.6 , S3: 1255.9 - 1424.4
#pick which borehole to feature in plot 2:
BOREHOLE_NAME <- "S1"
BOREHOLE_LAF_MIN <- 230.3
BOREHOLE_LAF_MAX <- 388.8

# drops temps that seem insane?
#TMP_MIN <- -20
#TMP_MAX <- 120


#load and clean
df <- read.csv(CSV_PATH, stringsAsFactors = FALSE)

df <- df %>%
  # keep only the in-ground portion (LAF > 0) and plausible temperatures
  filter(LAF > 0, TMP >= TMP_MIN, TMP <= TMP_MAX) %>%
  # turn the text timestamp into a real datetime, and pull out a short label
  mutate(
    datetime = ymd_hms(start_time, tz = "UTC"),
    time_label = format(datetime, "%H:%M")
  )


## plot 1

p1 <- ggplot(df, aes(x = TMP, y = LAF, group = start_time, color = time_label)) +
  geom_path(linewidth = 0.3) +
  scale_y_reverse() +
  labs(x = "Temperature (\u00B0C)", y = "Length along fiber (m)",
       color = "Time (UTC)",
       title = paste0("Channel ", df$channel[1], " — full in-ground fiber")) +
  theme_bw()

print(p1)

## plot 2 

bh <- df %>%
  filter(LAF > BOREHOLE_LAF_MIN, LAF < BOREHOLE_LAF_MAX) %>%
  mutate(depth_proxy = LAF - min(LAF))   # 0 at borehole top

p2 <- ggplot(bh, aes(x = TMP, y = depth_proxy,
                     group = start_time, color = time_label)) +
  geom_path(linewidth = 0.8) +
  scale_y_reverse() +
  labs(x = "Temperature (\u00B0C)", y = "Depth proxy (m into borehole)",
       color = "Time (UTC)",
       title = paste0("Borehole ", BOREHOLE_NAME,
                      " (LAF ", BOREHOLE_LAF_MIN, "\u2013", BOREHOLE_LAF_MAX, " m)")) +
  theme_bw()

print(p2)


df_bored <- df %>%
  mutate(borehole = case_when(
    LAF > 230.3  & LAF < 388.8  ~ "S1",
    LAF > 775.1  & LAF < 933.6  ~ "S2",
    LAF > 1255.9 & LAF < 1424.4 ~ "S3",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(borehole)) %>%
  group_by(borehole) %>%
  mutate(depth_proxy = LAF - min(LAF)) %>%
  ungroup()

p3 <- ggplot(df_bored, aes(x = TMP, y = depth_proxy,
                           group = start_time, color = time_label)) +
  geom_path(linewidth = 0.6) +
  scale_y_reverse() +
  facet_grid(cols = vars(borehole)) +
  labs(x = "Temperature (\u00B0C)", y = "Depth proxy (m)",
       color = "Time (UTC)",
       title = "Downhole profiles by borehole") +
  theme_bw()

print(p3)


####LOOKING FOR XML FILE LOCATIONS 
library(DBI); library(RPostgres)
con <- dbConnect(RPostgres::Postgres(),
                 dbname="dts_db", host="dts.physics.carleton.edu", port="5432",
                 user="dts_user_ro", password="$$N0th1ng5p3c14l$$")

dbListTables(con)

dbListFields(con, "measurement") 
dbListFields(con, "dts_config")
dbListFields(con, "dts_data")



install.packages("googledrive")
library(googledrive)
#need to run this to give R access to your drive
drive_auth()   
shared <- drive_find(q = "sharedWithMe = true")
print(shared, n = 50)


folder <- drive_get(as_id("12tkE_ITIb1XxKthTA4BVB965zs5w4sTe"))

folder <- drive_get(as_id("12tkE_ITIb1XxKthTA4BVB965zs5w4sTe"))
files <- drive_ls(folder, pattern = "\\.xml$")
nrow(files)

