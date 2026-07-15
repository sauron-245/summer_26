############################################################
#  Bald Spot DTS — ANALYSES
#
#  Run 01_setup.R FIRST (once per session). Then run any section below
#  independently — each has its own settings and its own dataset, so they
#  don't step on each other.
#
#    A. depth profiles — 15th of each month
#    B. month comparison — same month across years (are Januaries changing?)
#    C. trend over time — mean borehole temperature per day
############################################################

library(dplyr)
library(lubridate)
library(ggplot2)


############################################################
#  A. DEPTH PROFILES — 15th of each month
############################################################

# --- settings ---
a_borehole <- "S1"
a_day      <- 15      # day of month
a_hour     <- 12      # hour (UTC) to sample near
a_years_db <- c(2019, 2020, 2021, 2022, 2023)   # database years to include

# --- build ---
a_laf <- BOREHOLE_LAF[[a_borehole]]

a_db <- get_db_data(
  sprintf("date_part('day', M.datetime_utc) = %d
           AND date_part('year', M.datetime_utc) IN (%s)",
          a_day, paste(a_years_db, collapse = ",")),
  a_laf[1], a_laf[2]) %>%
  prep_db(a_hour)

a_xml <- fetch_and_parse(pick_xml(day == a_day, a_hour)) %>%
  add_boreholes() %>% add_depth(channel_maps) %>% prep_xml(a_borehole)

a_data <- bind_rows(a_db, a_xml) %>%
  mutate(month = factor(month(datetime)))

# --- plot ---
ggplot(a_data, aes(temperature_c, depth_m, group = obs_id, color = month)) +
  geom_path(aes(linewidth = source, alpha = source)) +
  scale_y_reverse() +
  coord_cartesian(xlim = c(5, 20)) +
  scale_color_viridis_d(option = "viridis") +
  scale_linewidth_manual(values = c(database = 0.4, xml = 0.4)) +
  scale_alpha_manual(values = c(database = 0.9, xml = 0.35)) +
  labs(x = "Temperature (°C)", y = "Depth (m)",
       color = "month", linewidth = "source", alpha = "source",
       title = paste0(a_borehole, " — day ", a_day, " of each month")) +
  theme_bw()


############################################################
#  B. MONTH COMPARISON — same month across years
############################################################

# --- settings ---
b_borehole <- "S1"
b_months   <- c(6)     # which months to compare; e.g. c(1) or c(1,4,7,10)
b_day      <- 15
b_hour     <- 12
rm(list = intersect(c("b_db", "b_xml", "b_data"), ls()))

# --- build ---
b_laf <- BOREHOLE_LAF[[b_borehole]]

b_db <- get_db_data(
  sprintf("date_part('day', M.datetime_utc) = %d
           AND date_part('month', M.datetime_utc) IN (%s)",
          b_day, paste(b_months, collapse = ",")),
  b_laf[1], b_laf[2]) %>%
  prep_db(b_hour)

b_picks <- pick_xml(day == b_day & month %in% b_months, b_hour)
cat("xml: selected", nrow(b_picks), "files\n")

b_xml <- if (nrow(b_picks) > 0) {
  fetch_and_parse(b_picks) %>%
    add_boreholes() %>% add_depth(channel_maps) %>% prep_xml(b_borehole)
} else {
  NULL
}

b_data <- bind_rows(b_db, b_xml) %>%
  mutate(year = factor(year(datetime)),
         month_name = month(datetime, label = TRUE, abbr = FALSE))

# what years did you get?
b_data %>% distinct(month_name, year, source) %>% arrange(month_name, year) %>% print(n = 50)

# --- plot: facet by month, color by year ---
p <- ggplot(b_data, aes(temperature_c, depth_m, group = obs_id,
                        color = year, linetype = source)) +
  geom_path(linewidth = 0.5) +
  scale_y_reverse() +
  coord_cartesian(xlim = c(5, 20)) +
  scale_color_viridis_d(option = "viridis") +
  scale_linetype_manual(values = c(database = "solid", xml = "dashed")) +
  facet_wrap(~ month_name) +
  labs(x = "Temperature (°C)", y = "Depth (m)",
       color = "year", linetype = "source",
       title = paste0(b_borehole, " — same month across years")) +
  theme_bw()

ggplotly(p)
############################################################
#  C. TREND OVER TIME — mean borehole temperature per day
############################################################

# --- settings ---
c_borehole   <- "S1"
c_xml_start  <- "2025-10-01"    # xml date range to include
c_xml_end    <- "2026-10-31"
c_hour       <- 12

# --- build ---
c_laf <- BOREHOLE_LAF[[c_borehole]]

# database: every observation on record (dense, makes the seasonal curve)
c_db <- get_db_data("TRUE", c_laf[1], c_laf[2]) %>%
  mutate(date = as_date(datetime_utc)) %>%
  group_by(date) %>%
  summarize(mean_temp = mean(temperature_c, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "database")

# xml: one observation per day across your range
c_xml <- fetch_and_parse(
  pick_xml(as_date(datetime) >= as_date(c_xml_start) &
             as_date(datetime) <= as_date(c_xml_end), c_hour)) %>%
  add_boreholes() %>% add_depth(channel_maps) %>%
  filter(borehole == c_borehole) %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarize(mean_temp = mean(TMP, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "xml")

c_data <- bind_rows(c_db, c_xml)

# --- plot ---
ggplot(c_data, aes(date, mean_temp, color = source)) +
  geom_line(data = filter(c_data, source == "database"), linewidth = 0.4) +
  geom_point(data = filter(c_data, source == "xml"), size = 1.0, shape = 17) +
  scale_color_manual(values = c(database = "darkgrey", xml = "red")) +
  labs(x = "Year", y = "Mean borehole temperature (°C)", color = NULL,
       title = paste0(c_borehole, " — mean daily temperature over time"),
       subtitle = "database and xml temperatures are produced differently — offsets may not be real change") +
  theme_bw() +
  theme(legend.position = "bottom")


#############################################################
#  D. PINCH POINT vs NON-PINCH — how do they change over time?
#
#  A pinch point is defined by LOW seasonal amplitude (groundwater flow pins it
#  near baseline). So the comparison that matters is amplitude, not absolute
#  temperature. Three views:
#    D1 — mean temperature of each interval over time (the raw trajectory)
#    D2 — yearly amplitude per interval (does the pinch stay pinned?)
#    D3 — deviation from baseline, distribution per interval per year
############################################################

# --- settings ---
d_borehole <- "S1"
d_day      <- 15      # sample the 15th of each month
d_hour     <- 12
d_years_db <- c(2019, 2020, 2021, 2022, 2023)

# the depth ranges you want to compare — edit these
DEPTH_INTERVALS <- list(
  pinch     = c(22.0, 20.7),
  non_pinch = c(29.8, 30.6)
)

# --- build: monthly profiles from both sources ---
d_laf <- BOREHOLE_LAF[[d_borehole]]

d_db <- get_db_data(
  sprintf("date_part('day', M.datetime_utc) = %d
           AND date_part('year', M.datetime_utc) IN (%s)",
          d_day, paste(d_years_db, collapse = ",")),
  d_laf[1], d_laf[2]) %>%
  prep_db(d_hour)

d_xml <- fetch_and_parse(pick_xml(day == d_day, d_hour)) %>%
  add_boreholes() %>% add_depth(channel_maps) %>% prep_xml(d_borehole)

d_all <- bind_rows(d_db, d_xml)

# slice out the intervals and compute metrics
d_intervals <- extract_all_intervals(d_all, DEPTH_INTERVALS)
d_metrics   <- interval_metrics(d_intervals)
d_amp       <- interval_amplitude(d_metrics, by = "year")

# sanity: how many observations per interval per year?
d_amp %>% arrange(interval, year) %>% print(n = 50)


# --- D1: mean temperature of each interval over time ------------------------
#  the pinch interval should hug the baseline line; the non-pinch should swing.

ggplot(d_metrics, aes(datetime, mean_temp, color = interval, shape = source)) +
  geom_point(size = 1.5) +
  geom_line(aes(group = interaction(interval, source)), linewidth = 0.3, alpha = 0.6) +
  geom_hline(yintercept = BASELINE_TEMP, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(pinch = "#1f6feb", non_pinch = "#d1242f")) +
  labs(x = NULL, y = "Mean temperature in interval (°C)",
       color = "interval", shape = "source",
       title = paste0(d_borehole, " — pinch vs non-pinch over time"),
       subtitle = "dashed line = assumed pre-operational baseline") +
  theme_bw()


# --- D2: yearly amplitude per interval (THE key comparison) -----------------
#  pinch points are defined by small amplitude. if the pinch interval's
#  amplitude grows over years, the groundwater pinning may be weakening.

ggplot(d_amp, aes(factor(year), amplitude, fill = interval)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c(pinch = "#1f6feb", non_pinch = "#d1242f")) +
  labs(x = "Year", y = "Peak-to-trough amplitude (°C)", fill = "interval",
       title = paste0(d_borehole, " — seasonal amplitude by interval, per year"),
       subtitle = "pinch points should show consistently LOW amplitude") +
  theme_bw()


# --- D3: deviation from baseline, distribution per year ---------------------
#  another view: how far each interval strays from baseline across the year.

ggplot(d_metrics, aes(factor(year), dev, fill = interval)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.6,
               outlier.size = 0.5) +
  scale_fill_manual(values = c(pinch = "#1f6feb", non_pinch = "#d1242f")) +
  labs(x = "Year", y = "|temperature − baseline| (°C)", fill = "interval",
       title = paste0(d_borehole, " — deviation from baseline by interval"),
       subtitle = "pinch points stay near zero; non-pinch strays further") +
  theme_bw()
