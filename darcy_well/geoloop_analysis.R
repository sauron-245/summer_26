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
parish = read_csv("dts_0706.csv")

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

dates = c("2026-07-23", "2026-07-24", "2026-07-25")

P1 = geoloop %>% 
  filter(date(TIMESTAMP) %in% dates) %>%
  ggplot() + 
  geom_line(aes(x = TIMESTAMP, y = GeoloopToBldgT)) +
  theme_bw(base_size = 15) +
  labs(title = "Production Well Return Water Temperature", x = "Date/Time", y = "Water Temp (Deg. C)")

P4 = geoloop %>% 
  filter(date(TIMESTAMP) %in% dates) %>%
  ggplot() + 
  geom_line(aes(x = TIMESTAMP, y = GeoloopGPM)) +
  theme_bw(base_size = 15) +
  labs(title = "Geoloop Flow Rate", x = "Date/Time", y = "Flow Rate (GPM")

P2 = zentra %>% 
  filter(date(TIMESTAMP) %in% dates) %>%
  ggplot(aes(x = TIMESTAMP, y = water_level_m)) + 
  geom_line() +
  # geom_smooth(method = 'gam', se = FALSE, color = 'blue', formula = y ~ s(x, k = 40, bs = "cs")) +
  theme_bw(base_size = 15) +
  labs(title = "Observation Well Water Height", x = "Date/Time", y = "Water Height (M)")

P3 = zentra %>% 
  filter(date(TIMESTAMP) %in% dates) %>%
  ggplot(aes(x = TIMESTAMP, y  = water_temp)) + 
  geom_line() +
  # geom_smooth(method = 'gam', se = FALSE, color = 'blue', formula = y ~ s(x, k = 40, bs = "cs")) +
  theme_bw(base_size = 15) +
  labs(title = "Observation Well Water Temperature", x = "Date/Time", y = "Water Temp (Deg. C)")

P7 = zentra %>% 
  filter(date(TIMESTAMP) %in% dates) %>%
  ggplot(aes(x = TIMESTAMP, y  = spc)) + 
  geom_line() +
  # geom_smooth(method = 'gam', se = FALSE, color = 'blue', formula = y ~ s(x, k = 40, bs = "cs")) +
  theme_bw(base_size = 15) +
  labs(title = "Observation Well Specific Conductivity", x = "Date/Time", y = "SPC (Us/cm)")

# P5 = parish %>%
#   # ggplot(aes(x = datetime, y = TMP)) +
#   geom_line() +
#   theme_bw() +
#   labs(title = "DTS Fiber Temp", x = "Date/Time", y = "Water Temp (Deg. C)")

P6 = geoloop %>% 
  filter(date(TIMESTAMP) %in% dates) %>%
  ggplot() + 
  geom_line(aes(x = TIMESTAMP, y = VFDFreq)) +
  theme_bw(base_size = 15) +
  labs(title = "Production Well Pump Frequency (30 = ON)", x = "Date/Time", y = "Signal Frequency")

P7 / P3 / P1 / P6  + plot_layout(axes = "collect")

ggplot(zentra, aes(x = water_level_m, y = water_temp, color = TIMESTAMP)) + 
  geom_jitter() + 
  # geom_smooth(method = 'loess', se = FALSE) +
  theme_bw()

  
  