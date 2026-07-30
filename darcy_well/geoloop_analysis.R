library(tidyverse)
library(readxl)
library(lubridate)

geoloop = read_excel("geoloop.xlsx") %>% 
  mutate(TIMESTAMP = as_datetime(TIMESTAMP))

geoloop %>% 
  mutate(diff = GeoloopToBldgT - GeoloopFromBldgT) %>% 
  # filter(date(TIMESTAMP) == "2026-07-01") %>%
  ggplot() +
  # geom_line(aes(x = TIMESTAMP, y = VFDFreq * 2, color = 'VFD Freq')) +
  # geom_line(aes(x = TIMESTAMP, y = GeoloopFromBldgT, color = 'Loop `from` building temp')) +
  # geom_line(aes(TIMESTAMP, GeoloopToBldgT, color = 'Loop `to` building temp')) +
  geom_line(aes(TIMESTAMP, GeoloopGPM, color = 'Flow rate (GPM)'), size =  0.5) +
  # geom_smooth(aes(TIMESTAMP, GeoloopGPM /2, color = 'Flow rate (GPM)'), method = "gam", se = FALSE) +
  # geom_hline(yintercept = 0) +
  # scale_y_continuous(sec.axis = sec_axis(~. / 2)) +
  theme_bw() +
  labs(title = "sum bs idk")
