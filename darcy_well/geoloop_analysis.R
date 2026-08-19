library(tidyverse)
library(readxl)
library(lubridate)
library(patchwork)
library(RColorBrewer)

geoloop = read_excel("geoloop.xlsx") %>% 
  mutate(TIMESTAMP = as_datetime(TIMESTAMP), GeoloopToBldgT = (GeoloopToBldgT - 32) * 5/9, GeoloopFromBldgT = (GeoloopFromBldgT - 32) * 5/9) %>% 
  filter(GeoloopToBldgT <= 20) %>% 
  mutate(datetime_round = round_date(TIMESTAMP, "15 mins"))
zentra = read_csv("zentra.csv") %>% 
  rename(TIMESTAMP = `z6-22205`, water_level_m = `Port1...2`, water_temp = `Port1...3`, spc = `Port1...4`) %>% 
  mutate(water_level_m = as.numeric(water_level_m) / 1000, water_temp = as.numeric(water_temp), TIMESTAMP = mdy_hms(TIMESTAMP), spc = as.numeric(spc) * 1000) %>% 
  slice(3:n()) %>% 
  select(1:4) %>% 
  filter(TIMESTAMP >= "2026-06-30 00:01:00", TIMESTAMP <= "2026-07-27 23:59:00")%>% 
  mutate(datetime_round = round_date(TIMESTAMP, "15 mins"))
parish = read_csv("parish_dts_xdepth.csv") %>% 
  mutate(dt_cst = datetime - hours(3))%>% 
  mutate(datetime_round = round_date(dt_cst, "15 mins"))
parish_pre <- read_csv("pre_op.csv")%>% 
  mutate(dt_cst = datetime - hours(3))


my_pal = brewer.pal(n = 11, name = "Spectral")

shade_periods <- geoloop %>%
  arrange(TIMESTAMP) %>%
  mutate(
    shaded = VFDFreq == 30,
    grp = cumsum(shaded != lag(shaded, default = first(shaded)))
  ) %>%
  filter(shaded) %>%
  group_by(grp) %>%
  summarize(
    start = min(TIMESTAMP),
    end = max(TIMESTAMP),
    .groups = "drop"
  )

llim1 = as.POSIXct("2026-07-06 0:00:00")
ulim1 = as.POSIXct("2026-07-09 0:00:00")
llim2 = as.POSIXct("2026-07-23 0:00:00")
ulim2 = as.POSIXct("2026-07-26 0:00:00")





P1 =
  ggplot(df_full1) + 
  geom_line(aes(x = TIMESTAMP, y = GeoloopToBldgT), linewidth = 1) +
  annotate(
    geom = "rect",
    xmin = llim1,
    xmax = ulim1,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.2
  ) +
  annotate(
    geom = "rect",
    xmin = llim2,
    xmax = ulim2,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.2
  ) +
  geom_rect(data = shade_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "lightblue1", alpha = 0.4) +
  theme_bw(base_size = 15) +
  # scale_x_datetime(limits = c(llim, ulim)) +
  labs(title = "Production Well Return Water Temperature", x = "Date", y = "Water Temp (Deg. C)")

P4 =
  ggplot(df_full1) + 
  geom_line(aes(x = TIMESTAMP, y = GeoloopGPM), linewidth = 1) +
  geom_rect(data = shade_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "lightblue1", alpha = 0.4) +
  # scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15) +
  labs(title = "Geoloop Flow Rate", x = "Date", y = "Flow Rate (GPM")

P2 = 
  ggplot(df_full1) + 
  geom_line(aes(x = TIMESTAMP, y = water_level_m), linewidth = 1) +
  annotate(
    geom = "rect",
    xmin = llim1,
    xmax = ulim1,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.2
  ) +
  annotate(
    geom = "rect",
    xmin = llim2,
    xmax = ulim2,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.2
  ) +
  geom_rect(data = shade_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "lightblue1", alpha = 0.4) +
  # geom_smooth(method = 'gam', se = FALSE, color = 'blue', formula = y ~ s(x, k = 40, bs = "cs")) +
  # scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15) +
  labs(title = "Observation Well Water Height", x = "Date", y = "Water Height (M)")

P3 =
  ggplot(df_full1) + 
  geom_line(aes(x = TIMESTAMP, y  = water_temp), linewidth = 1) +
  annotate(
    geom = "rect",
    xmin = llim1,
    xmax = ulim1,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.2
  ) +
  annotate(
    geom = "rect",
    xmin = llim2,
    xmax = ulim2,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.2
  ) +
  geom_rect(data = shade_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "lightblue1", alpha = 0.4) +
  # geom_smooth(method = 'gam', se = FALSE, color = 'blue', formula = y ~ s(x, k = 40, bs = "cs")) +
  # scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15) +
  labs(title = "Observation Well Water Temperature", x = "Date", y = "Water Temp (Deg. C)")

P7 = 
  ggplot(df_full1) + 
  geom_line(aes(x = TIMESTAMP, y  = spc), linewidth = 1) +
  geom_rect(data = shade_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "lightblue1", alpha = 0.4) +
  scale_x_datetime(limits = c(llim, ulim)) +
  # geom_smooth(method = 'gam', se = FALSE, color = 'blue', formula = y ~ s(x, k = 40, bs = "cs")) +
  theme_bw(base_size = 15) +
  labs(title = "Observation Well Specific Conductivity", x = "Date", y = "SPC (Us/cm)")

parish_jul6 = ggplot() + 
  geom_line(data = parish %>% filter(depth_m == 22.622), aes(x = dt_cst, y = TMP, color = 'Depth = 75 ft.'), linewidth = 1) +
  geom_line(data = parish %>% filter(depth_m == 38.128), aes(x = dt_cst, y = TMP, color = 'Depth = 125 ft.'), linewidth = 1) +
  geom_line(data = parish %>% filter(depth_m == 53.380), aes(x = dt_cst, y = TMP, color = 'Depth = 175 ft.'), linewidth = 1) +
  geom_line(data = parish %>% filter(depth_m == 68.632), aes(x = dt_cst, y = TMP, color = 'Depth = 225 ft.'), linewidth = 1) +
  scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15) + 
  scale_color_manual(breaks = c("Depth = 75 ft.", "Depth = 125 ft.", "Depth = 175 ft.", "Depth = 225 ft."), values = my_pal[8:11]) +
  geom_rect(data = shade_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "lightblue1", alpha = 0.4) +
  labs(x = "Date", y = "Water Temp (deg. C)", title = "Obs. Well Temperature (DTS Cable)", color = "Length Along Fiber")

parish_jul23 = ggplot() + 
  geom_line(data = parish %>% filter(depth_m == 22.622), aes(x = dt_cst, y = TMP, color = 'Depth = 75 ft.'), linewidth = 1) +
  geom_line(data = parish %>% filter(depth_m == 38.128), aes(x = dt_cst, y = TMP, color = 'Depth = 125 ft.'), linewidth = 1) +
  geom_line(data = parish %>% filter(depth_m == 53.380), aes(x = dt_cst, y = TMP, color = 'Depth = 175 ft.'), linewidth = 1) +
  geom_line(data = parish %>% filter(depth_m == 68.632), aes(x = dt_cst, y = TMP, color = 'Depth = 225 ft.'), linewidth = 1) +
  scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15) + 
  scale_color_manual(breaks = c("Depth = 75 ft.", "Depth = 125 ft.", "Depth = 175 ft.", "Depth = 225 ft."), values = my_pal[8:11]) +
  geom_rect(data = shade_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "lightblue1", alpha = 0.4) +
  labs(x = "Date", y = "Water Temp (deg. C)", title = "Obs. Well Temperature (DTS Cable)", color = "Length Along Fiber")


P1 / P3 / P2 + plot_layout(axes = "collect")

P2

parish_jul6

ggplot(zentra, aes(x = water_level_m, y = water_temp, color = TIMESTAMP)) + 
  geom_jitter() + 
  # geom_smooth(method = 'loess', se = FALSE) +
  theme_bw()

parish_jun6 = ggplot() + 
  geom_line(data = parish_pre %>% filter(depth_m == 22.622), aes(x = dt_cst, y = TMP, color = 'Depth = 75 ft.'), linewidth = 1) +
  geom_line(data = parish_pre %>% filter(depth_m == 38.128), aes(x = dt_cst, y = TMP, color = 'Depth = 125 ft.'), linewidth = 1) +
  geom_line(data = parish_pre %>% filter(depth_m == 53.380), aes(x = dt_cst, y = TMP, color = 'Depth = 175 ft.'), linewidth = 1) +
  geom_line(data = parish_pre %>% filter(depth_m == 68.632), aes(x = dt_cst, y = TMP, color = 'Depth = 225 ft.'), linewidth = 1) +
  scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15) + 
  scale_color_manual(breaks = c("Depth = 75 ft.", "Depth = 125 ft.", "Depth = 175 ft.", "Depth = 225 ft."), values = c("skyblue", "goldenrod", "dodgerblue4", "tomato3")) +
  labs(x = "Date", y = "Water Temp (deg. C)", title = "Obs. Well Temperature (DTS Cable)", color = "Length Along Fiber")
parish_jun6  


dts_x_zentra <- ggplot() + 
  geom_line(data = zentra, aes(x = TIMESTAMP, y = water_temp, color = "Zentra")) + 
  geom_line(data = parish %>% filter(depth_m == 22.622), aes(x = dt_cst, y = TMP, color = "DTS")) + 
  scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15)
dts_x_zentra  



df_full <- zentra %>% full_join(parish, by = "datetime_round")
df_full <- df_full %>% full_join(geoloop, by = "datetime_round")  
df_full1 <- df_full %>% select(-c(1, 18, 19, 20, 21))

write_csv(df_full1, "darcy_data_ALL.csv")
