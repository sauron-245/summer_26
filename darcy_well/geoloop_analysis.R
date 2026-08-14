library(tidyverse)
library(readxl)
library(lubridate)
library(patchwork)

geoloop = read_excel("geoloop.xlsx") %>% 
  mutate(TIMESTAMP = as_datetime(TIMESTAMP), GeoloopToBldgT = (GeoloopToBldgT - 32) * 5/9, GeoloopFromBldgT = (GeoloopFromBldgT - 32) * 5/9) %>% 
  filter(GeoloopToBldgT <= 20)
zentra = read_csv("zentra.csv") %>% 
  rename(TIMESTAMP = `z6-22205`, water_level_m = `Port1...2`, water_temp = `Port1...3`, spc = `Port1...4`) %>% 
  mutate(water_level_m = as.numeric(water_level_m) / 1000, water_temp = as.numeric(water_temp), TIMESTAMP = mdy_hms(TIMESTAMP), spc = as.numeric(spc) * 1000) %>% 
  slice(3:n()) %>% 
  select(1:4) %>% 
  filter(TIMESTAMP >= "2026-06-30 00:01:00", TIMESTAMP <= "2026-07-27 23:59:00")
parish = read_csv("parish_dts_xdepth.csv") %>% 
  mutate(dt_cst = datetime - hours(3))
parish_pre <- read_csv("pre_op.csv")%>% 
  mutate(dt_cst = datetime - hours(3))

geoloop %>% 
  # mutate(diff = GeoloopToBldgT - GeoloopFromBldgT) %>% 
  filter(date(TIMESTAMP) == "2026-07-16") %>%
  ggplot() +
  # geom_line(aes(x = TIMESTAMP, y = VFDFreq * 2, color = 'VFD Freq')) +
  geom_line(aes(x = TIMESTAMP, y = GeoloopFromBldgT, color = 'Loop `from` building temp')) +
  geom_line(aes(TIMESTAMP, GeoloopToBldgT, color = 'Loop `to` building temp')) +
  # geom_line(aes(TIMESTAMP, GeoloopGPM, color = 'Flow rate (GPM)'), size =  0.5) +
  # geom_smooth(aes(TIMESTAMP, GeoloopGPM /2, color = 'Flow rate (GPM)'), method = "gam", se = FALSE) +
  # geom_hline(yintercept = 0) +
  # scale_y_continuous(sec.axis = sec_axis(~. / 2)) +
  theme_bw() 
  # labs(title = "sum bs idk")

ulim = as.POSIXct("2026-07-06 0:00:00")
llim = as.POSIXct("2026-07-08 0:00:00")

P1 = geoloop %>% 
  # filter(date(TIMESTAMP) %in% dates) %>%
  # filter(TIMESTAMP <= ulim1, TIMESTAMP >= llim1) %>%
  ggplot() + 
  geom_line(aes(x = TIMESTAMP, y = GeoloopToBldgT), linewidth = 1) +
  theme_bw(base_size = 15) + 
  scale_x_datetime(limits = c(llim, ulim)) +
  labs(title = "Production Well Return Water Temperature", x = "Date", y = "Water Temp (Deg. C)")

P4 = geoloop %>% 
  # filter(date(TIMESTAMP) %in% dates) %>%
  # filter(TIMESTAMP <= ulim1, TIMESTAMP >= llim1) %>%
  ggplot() + 
  geom_line(aes(x = TIMESTAMP, y = GeoloopGPM), linewidth = 1) +
  scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15) +
  labs(title = "Geoloop Flow Rate", x = "Date", y = "Flow Rate (GPM")

P2 = zentra %>% 
  # filter(date(TIMESTAMP) %in% dates) %>%
  # filter(TIMESTAMP <= ulim1, TIMESTAMP >= llim1) %>%
  ggplot(aes(x = TIMESTAMP, y = water_level_m)) + 
  geom_line(linewidth = 1) +
  # geom_smooth(method = 'gam', se = FALSE, color = 'blue', formula = y ~ s(x, k = 40, bs = "cs")) +
  scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15) +
  labs(title = "Observation Well Water Height", x = "Date", y = "Water Height (M)")

P3 = zentra %>% 
  # filter(date(TIMESTAMP) %in% dates) %>%
  # filter(TIMESTAMP <= ulim1, TIMESTAMP >= llim1) %>%
  ggplot(aes(x = TIMESTAMP, y  = water_temp)) + 
  geom_line(linewidth = 1) +
  # geom_smooth(method = 'gam', se = FALSE, color = 'blue', formula = y ~ s(x, k = 40, bs = "cs")) +
  scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15) +
  labs(title = "Observation Well Water Temperature", x = "Date", y = "Water Temp (Deg. C)")

P7 = zentra %>% 
  # filter(date(TIMESTAMP) %in% dates) %>%
  # filter(TIMESTAMP <= ulim1, TIMESTAMP >= llim1) %>%
  ggplot(aes(x = TIMESTAMP, y  = spc)) + 
  geom_line(linewidth = 1) +
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
  scale_color_manual(breaks = c("Depth = 75 ft.", "Depth = 125 ft.", "Depth = 175 ft.", "Depth = 225 ft."), values = c("skyblue", "goldenrod", "dodgerblue4", "tomato3")) +
  labs(x = "Date", y = "Water Temp (deg. C)", title = "Obs. Well Temperature (DTS Cable)", color = "Length Along Fiber")

parish_jul23 = ggplot() + 
  geom_line(data = parish %>% filter(depth_m == 22.622), aes(x = dt_cst, y = TMP, color = 'Depth = 75 ft.'), linewidth = 1) +
  geom_line(data = parish %>% filter(depth_m == 38.128), aes(x = dt_cst, y = TMP, color = 'Depth = 125 ft.'), linewidth = 1) +
  geom_line(data = parish %>% filter(depth_m == 53.380), aes(x = dt_cst, y = TMP, color = 'Depth = 175 ft.'), linewidth = 1) +
  geom_line(data = parish %>% filter(depth_m == 68.632), aes(x = dt_cst, y = TMP, color = 'Depth = 225 ft.'), linewidth = 1) +
  scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15) + 
  scale_color_manual(breaks = c("Depth = 75 ft.", "Depth = 125 ft.", "Depth = 175 ft.", "Depth = 225 ft."), values = c("skyblue", "goldenrod", "dodgerblue4", "tomato3")) +
    labs(x = "Date", y = "Water Temp (deg. C)", title = "Obs. Well Temperature (DTS Cable)", color = "Length Along Fiber")


P6 = geoloop %>% 
  # filter(date(TIMESTAMP) %in% dates) %>%
  # filter(TIMESTAMP <= ulim1, TIMESTAMP >= llim1) %>%
  ggplot() + 
  geom_line(aes(x = TIMESTAMP, y = VFDFreq), linewidth = 1) +
  scale_x_datetime(limits = c(llim, ulim)) +
  theme_bw(base_size = 15) +
  labs(title = "Production Well Pump Frequency", x = "Date", y = "Signal Frequency")

P6 / P2 / P3 / parish_jul6  + plot_layout(axes = "collect")

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
