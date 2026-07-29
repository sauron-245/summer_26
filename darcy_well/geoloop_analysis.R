library(tidyverse)
library(readxl)
library(lubridate)

geoloop = read_excel("geoloop.xlsx") %>% 
  mutate(TIMESTAMP = as_datetime(TIMESTAMP))

geoloop %>% 
  mutate(diff = GeoloopToBldgT - GeoloopFromBldgT) %>% 
  filter(date(TIMESTAMP) == "2026-06-30") %>%
  ggplot(aes(x = TIMESTAMP, y = GeoloopFromBldgT, color = 'Loop `from` building temp')) + 
  geom_line() + 
  geom_line(aes(TIMESTAMP, GeoloopToBldgT, color = 'Loop `to` building temp')) +
  geom_line(aes(TIMESTAMP, GeoloopGPM, color = 'Flow rate (GPM)')) +
  # geom_smooth(aes(TIMESTAMP, diff, color = 'difference')) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(title = "sum bs idk")
