############################################################
#  Darcy Well — INTERACTIVE DASHBOARD (Shiny)
#
#  HOW TO RUN:
#   1. Run the SETUP + FETCH SETTINGS block below (edit what you want the app to pull).
#   2. It downloads and parses the selected files once.
#   3. The app launches — toggle observations, legs, depth units, etc. instantly.
############################################################

library(googledrive)
library(xml2)
library(dplyr)
library(tibble)
library(lubridate)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)


############################################################
#  FIXED WELL SETTINGS (shouldn't need to touch)
############################################################

FOLDER_ID       <- "1wQGImxaQGcEA_156JJBwIDPLrG86JfZQ"
TURNAROUND_LAF  <- 83.5   # lowest point of the well (cable turnaround), m along fiber
TOP_OF_WELL_LAF <- 8      # LAF where the cable enters the top of the well
TMP_MIN <- -10
TMP_MAX <- 100


############################################################
#  FUNCTIONS (shouldn't need to touch)
############################################################

build_file_index <- function(folder_id) {
  items <- drive_find(q = sprintf("'%s' in parents", folder_id),
                      corpus = "allDrives")
  items <- items[grepl("\\.xml$", items$name, ignore.case = TRUE), ]
  tibble(name = items$name, id = items$id) %>%
    mutate(
      date_str = str_match(name, "_(\\d{8})_")[, 2],
      time_str = str_match(name, "_\\d{8}_(\\d{6})")[, 2],
      datetime = ymd_hms(paste0(date_str, time_str), tz = "UTC"),
      year = year(datetime), month = month(datetime), day = day(datetime),
      hour = hour(datetime), minute = minute(datetime)
    )
}

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

fetch_and_parse <- function(selected) {
  if (nrow(selected) == 0) stop("no files selected — check your fetch settings")
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

# folds each profile: down/up legs, real depth (m and ft), drops out-of-well junk
fold_profile <- function(df) {
  well_bottom_laf <- TURNAROUND_LAF + (TURNAROUND_LAF - TOP_OF_WELL_LAF)
  df %>%
    filter(TMP > TMP_MIN, TMP < TMP_MAX,
           LAF >= TOP_OF_WELL_LAF, LAF <= well_bottom_laf) %>%
    mutate(
      datetime = ymd_hms(start_time, tz = "UTC"),
      label = format(datetime, "%Y-%m-%d %H:%M"),
      leg = if_else(LAF <= TURNAROUND_LAF, "down", "up"),
      depth_m = if_else(LAF <= TURNAROUND_LAF,
                        LAF - TOP_OF_WELL_LAF,
                        2 * TURNAROUND_LAF - LAF - TOP_OF_WELL_LAF),
      depth_ft = depth_m * 3.28084
    ) %>%
    filter(depth_m >= 0)
}


############################################################
#  Controls which files get downloaded and parsed for the app.
#  Pick one fetch_mode:
#    "time_window"  every observation between two hours on one day
#    "date_range"   one obs/day (near target_hour) across a date range
#    "nth_of_month" one obs (near target_hour) on the Nth of each month in a range
#    "single_day"   every observation on one day
############################################################

fetch_mode <- "single day"

target_hour <- 18            # hour (UTC) to sample near, for the thinning modes

# time_window:
window_day        <- "2026-07-07"
window_start_hour <- 17
window_end_hour   <- 19

# date_range:
range_start <- "2026-07-07"
range_end   <- "2026-07-07"

# nth_of_month:
nth_day      <- 15
nth_start    <- "2025-11-01"
nth_end      <- "2026-07-31"

# single_day:
single_day  <- "2026-07-07"

############################################################
# (you shouldn't need to edit below)
############################################################

select_files <- function(index) {
  if (fetch_mode == "time_window") {
    return(index %>%
             filter(as_date(datetime) == as_date(window_day),
                    hour >= window_start_hour, hour < window_end_hour) %>%
             arrange(datetime))
    
  } else if (fetch_mode == "single_day") {
    return(index %>%
             filter(as_date(datetime) == as_date(single_day)) %>%
             arrange(datetime))
    
  } else if (fetch_mode == "date_range") {
    base <- index %>% filter(as_date(datetime) >= as_date(range_start),
                             as_date(datetime) <= as_date(range_end))
    
  } else if (fetch_mode == "nth_of_month") {
    base <- index %>% filter(day == nth_day,
                             as_date(datetime) >= as_date(nth_start),
                             as_date(datetime) <= as_date(nth_end))
  } else {
    stop("fetch_mode must be time_window, date_range, nth_of_month, or single_day")
  }
  
  # thinning modes: one obs per day nearest target_hour
  base %>%
    mutate(date_only = as_date(datetime),
           mins_from_target = abs((hour * 60 + minute) - target_hour * 60)) %>%
    group_by(date_only) %>%
    slice_min(mins_from_target, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-date_only, -mins_from_target)
}


############################################################
#  RUN THE FETCH (the slow part)
############################################################

drive_auth()
index <- build_file_index(FOLDER_ID)
cat("indexed", nrow(index), "files, spanning",
    format(min(index$datetime, na.rm = TRUE)), "to",
    format(max(index$datetime, na.rm = TRUE)), "\n")

picks <- select_files(index)
cat("fetching", nrow(picks), "files...\n")
folded <- fetch_and_parse(picks) %>% fold_profile()

folded <- folded %>%
  mutate(date_label = format(datetime, "%Y-%m-%d"),
         time_label = format(datetime, "%H:%M"))

cat("ready:", length(unique(folded$label)), "observations loaded\n")


############################################################
#  THE APP
############################################################

ui <- fluidPage(
  titlePanel("Darcy Well — temperature vs depth"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 radioButtons("depth_unit", "Depth unit:",
                              choices = c("feet", "meters"), selected = "feet"),
                 checkboxGroupInput("legs", "Cable legs to show:",
                                    choices = c("down", "up"), selected = "down"),
                 radioButtons("leg_style", "Up-leg line style:",
                              choices = c("solid", "dotted", "dashed"), selected = "solid"),
                 selectInput("colorby", "Color lines by:",
                             choices = c("observation" = "label",
                                         "date" = "date_label",
                                         "time" = "time_label"),
                             selected = "label"),
                 tags$hr(),
                 checkboxGroupInput("which_obs", "Observations to show:",
                                    choices = sort(unique(folded$label)),
                                    selected = sort(unique(folded$label))),
                 sliderInput("temp_range", "Temperature range (°C):",
                             min = floor(min(folded$TMP)), max = ceiling(max(folded$TMP)),
                             value = c(10, 12), step = 0.1)
    ),
    mainPanel(width = 9,
              plotlyOutput("well_plot", height = "800px")
    )
  )
)

server <- function(input, output) {
  
  filtered <- reactive({
    folded %>%
      filter(leg %in% input$legs, label %in% input$which_obs)
  })
  
  output$well_plot <- renderPlotly({
    d <- filtered()
    validate(need(nrow(d) > 0, "No data — check your leg / observation filters."))
    
    d$y     <- if (input$depth_unit == "feet") d$depth_ft else d$depth_m
    d$color_var <- as.factor(d[[input$colorby]])
    y_lab   <- if (input$depth_unit == "feet") "Depth below well top (ft)" else "Depth below well top (m)"
    
    p <- ggplot(d, aes(TMP, y, color = color_var, linetype = leg,
                       group = interaction(start_time, leg),
                       text = paste0("depth: ", round(y, 1),
                                     "<br>temp: ", round(TMP, 2), " °C",
                                     "<br>", label))) +
      geom_path(linewidth = 0.6) +
      scale_y_reverse() +
      coord_cartesian(xlim = input$temp_range) +
      scale_linetype_manual(values = c(down = "solid", up = input$leg_style),
                            guide = "none") +
      scale_color_viridis_d(option = "viridis") +
      labs(x = "Temperature (°C)", y = y_lab, color = NULL,
           title = "Darcy Observation Well") +
      theme_bw()
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)