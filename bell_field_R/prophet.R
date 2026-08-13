# install.packages(prophet)
# install.packages(pracma)

library(tidyverse)
library(prophet)
library(lubridate)
library(pracma)

p1 = read_csv("hobos/hobos_running/P1_current.csv") %>% drop_na() %>% slice(3:n())

to_filter = p1$'Specific Conductivity'
result = hampel(to_filter, k = 100, t0 = 2)
final = p1 %>% slice(-result$ind)

ggplot(final, aes(x = `Date-Time`, y = `Specific Conductivity`)) +
  geom_line() +
  theme_minimal(base_size = 15)

data = final %>% filter(year(`Date-Time`) < 2025)%>% select(1, 4) %>% rename(ds = `Date-Time`, y = `Specific Conductivity`)
m = prophet(data)
