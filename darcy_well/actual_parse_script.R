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

#need to run this to give R access to your drive
#type 1 in terminal to grant access 
#go to browser and click continue
drive_auth()   
# shared <- drive_find(q = "sharedWithMe = true")
# print(shared, n = 50)


folder <- drive_get(as_id("1wQGImxaQGcEA_156JJBwIDPLrG86JfZQ"))

# files <- drive_find(
#   q = "'1wQGImxaQGcEA_156JJBwIDPLrG86JfZQ' in parents",
#   corpus = "allDrives"
# )
# nrow(files)
# files



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

