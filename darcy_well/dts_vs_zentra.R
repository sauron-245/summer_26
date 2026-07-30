library(tidyverse)
library(lubridate)
data = read_csv("parish_data_thinned.csv")
# time = as.POSIXct("2026-07-03 18:20:15")
well_data = data %>% 
  # mutate(start_time = as.character(start_time)) %>%
  filter(LAF > 0, TMP > 0, TMP < 50)

zentra = read_csv("parish_data.csv")  %>% 
  slice(3:n()) %>%
  rename(datetime = `z6-22205`, temp = Port1...3) %>%
  mutate(datetime = as_datetime(datetime, format = "%m/%d/%Y %I:%M:%S %p"), temp = as.numeric(temp)) %>%
  select(datetime, temp) %>%
  filter(day(datetime) <= 04)

graphing_data = data %>% filter(LAF == 74.073, month(start_time) == 07, day(start_time) >= 01, day(start_time) <= 05)

ggplot(zentra, aes(x = datetime, y= temp, color = "zentra")) + 
  geom_path() + 
  geom_smooth(method = "gam", se = FALSE) +
  geom_line(data = graphing_data, aes(x = start_time, y = TMP, color = "74 ft")) + 
  geom_smooth(data = graphing_data, aes(x = start_time, y = TMP, color = "74 ft"), method = "gam", se = FALSE) +
  theme_bw() + 
  labs(title = "Temperature from DTS and Zentra server, LAF = 74 ft.", x = "Date & Time", y = "Temperature (deg. C)", color = "Data Source")

