############################################################
#  Bald Spot DTS — INTERACTIVE DASHBOARD (Shiny)
#
#  Run 01_setup.R FIRST. Then run this whole file — it fetches everything once
#  (slow), then launches an interactive app where all toggling is instant.
#  You choose which variable facets the plots; the rest act as filters.
############################################################

library(shiny)
library(plotly)
library(dplyr)
library(lubridate)


############################################################
#  ONE-TIME FETCH — builds a master data frame of everything
############################################################

MASTER_DAY  <- 15
MASTER_HOUR <- 12
MASTER_DB_YEARS <- 2019:2023
ALL_BOREHOLES <- c("S1", "S2", "S3", "S4", "S5")

borehole_channel <- function(bh) if (bh %in% c("S1","S2","S3")) "1" else "3"

message("Building master dataset — this is the slow part...")

master <- bind_rows(lapply(ALL_BOREHOLES, function(bh) {
  ch  <- borehole_channel(bh)
  laf <- BOREHOLE_LAF[[bh]]
  
  db <- get_db_data(
    sprintf("date_part('day', M.datetime_utc) = %d
             AND date_part('year', M.datetime_utc) IN (%s)",
            MASTER_DAY, paste(MASTER_DB_YEARS, collapse = ",")),
    laf[1], laf[2], channel_name = paste("channel", ch)) %>%
    prep_db(MASTER_HOUR) %>%
    mutate(borehole = bh)
  
  picks <- pick_xml(day == MASTER_DAY, MASTER_HOUR, ch)
  xml <- if (nrow(picks) > 0) {
    fetch_and_parse(picks) %>%
      add_boreholes() %>% add_depth(channel_maps) %>%
      prep_xml(bh) %>% mutate(borehole = bh)
  } else NULL
  
  message("  done ", bh)
  bind_rows(db, xml)
}))

master <- master %>%
  mutate(year = year(datetime),
         month = month(datetime),
         month_name = month(datetime, label = TRUE, abbr = FALSE),
         date_label = format(datetime, "%Y-%m-%d"))

cat("master dataset:", nrow(master), "rows,",
    length(unique(master$obs_id)), "observations\n")


############################################################
#  THE APP
############################################################

ui <- fluidPage(
  titlePanel("Bald Spot DTS — depth profiles"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("facet_by", "Facet plots by:",
                             choices = c("borehole", "month", "year", "source"),
                             selected = "borehole"),
                 selectInput("colorby", "Color lines by:",
                             choices = c("month", "year", "date", "borehole", "source"),
                             selected = "month"),
                 tags$hr(),
                 checkboxGroupInput("boreholes", "Boreholes:",
                                    choices = ALL_BOREHOLES, selected = c("S1", "S4")),
                 checkboxGroupInput("months", "Months:",
                                    choices = setNames(1:12, month.name),
                                    selected = 1:12),
                 checkboxGroupInput("years", "Years:",
                                    choices = sort(unique(master$year)),
                                    selected = sort(unique(master$year))),
                 radioButtons("source", "Source:",
                              choices = c("both", "database", "xml"), selected = "both"),
                 tags$hr(),
                 radioButtons("xml_line", "XML line style:",
                              choices = c("solid", "dotted", "dashed"), selected = "solid"),
                 sliderInput("temp_range", "Temperature range (°C):",
                             min = 0, max = 25, value = c(5, 20), step = 0.5)
    ),
    mainPanel(width = 9,
              plotlyOutput("profile_plot", height = "800px")
    )
  )
)

server <- function(input, output) {
  
  filtered <- reactive({
    d <- master %>%
      filter(borehole %in% input$boreholes,
             month %in% as.numeric(input$months),
             year %in% as.numeric(input$years))
    if (input$source != "both") d <- d %>% filter(source == input$source)
    d
  })
  
  output$profile_plot <- renderPlotly({
    d <- filtered()
    validate(need(nrow(d) > 0, "No data for these settings — loosen the filters."))
    
    pick_var <- function(name) switch(name,
                                      month    = as.factor(d$month_name),
                                      year     = as.factor(d$year),
                                      date     = as.factor(d$date_label),
                                      borehole = as.factor(d$borehole),
                                      source   = as.factor(d$source))
    
    d$color_var <- pick_var(input$colorby)
    d$facet_var <- pick_var(input$facet_by)
    
    lt_values <- c(database = "solid", xml = input$xml_line)
    
    p <- ggplot(d, aes(temperature_c, depth_m, group = obs_id,
                       color = color_var, linetype = source,
                       text = paste0("depth: ", round(depth_m, 1), " m",
                                     "<br>temp: ", round(temperature_c, 2), " °C",
                                     "<br>", obs_id))) +
      geom_path(linewidth = 0.5) +
      scale_y_reverse() +
      coord_cartesian(xlim = input$temp_range) +
      scale_linetype_manual(values = lt_values) +
      facet_wrap(~ facet_var, nrow = 1) +
      labs(x = "Temperature (°C)", y = "Depth (m)",
           color = input$colorby, linetype = "source") +
      theme_bw()
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "v"))
  })
}

shinyApp(ui, server)