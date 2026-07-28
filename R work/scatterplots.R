setwd("~/GitHub/summer_26/R work")

library(tidyverse)
library(lubridate)
library(pracma)


## Set up continuous data

hobos_all <-  c("Dwnstrm", "P1", "P1_creek", "P2", "P2_creek", "P3", "Mdstrm", "Spring", "P4", "P4_creek", "P5", "P5_creek", "P5_creek_cond") 
hobos_spc <- c("Dwnstrm", "P1", "P2", "P3", "Mdstrm", "Spring", "P4", "P5", "P5_creek_cond") 
hobos_temp <-  c("P1_creek", "P2_creek", "P4_creek", "P5_creek") 


dfs <- list()
for (site in hobos_spc) {
  data <- read_csv(paste0("hobos/hobos_running/", site, "_current.csv"))
  data  <-  data %>% drop_na()
  spc  <-  data$`Specific Conductivity`
  temp <- data$Temperature
  result_spc <-  hampel(spc, k = 100, t0 = 2)
  result_temp <- hampel(temp, k = 100, t0 = 2)
  final  <-  data %>% slice(-c(result_spc$ind, result_temp$ind))
  dfs[[site]]  <-  final[, c("Date-Time", "Specific Conductivity", "Temperature")]
}

for (site in hobos_temp) {
  data <- read_csv(paste0("hobos/hobos_running/", site, "_current.csv"))
  data  <-  data %>% drop_na()
  temp <- data$Temperature
  result_temp <- hampel(temp, k = 100, t0 = 2)
  final  <-  data %>% slice(-result_temp$ind)
  
  dfs[[site]]  <-  final[, c("Date-Time", "Temperature")]
}

df_full <- bind_rows(dfs, .id = "Site") %>% 
  mutate(dt_round = round_date(`Date-Time`, unit = "15 ins"), date = as_date(date(`Date-Time`)))

rm(list = c("data", "dfs", "final", "result_spc", "result_temp", "spc", "temp", "site"))



## Set up manual data

manual_well_creek <- read_csv("manual_to_dl.csv") %>% 
  mutate(Site = case_when(
    Well == "P1" ~ "1",
    Well == "P2" ~ "2",
    Well == "P3" ~ "3",
    Well == "P4" ~ "4",
    Well == "P5" ~ "5",
    Well == "C1" ~ "1",
    Well == "C2" ~ "2",
    Well == "C3" ~ "3",
    Well == "C4" ~ "4",
    Well == "C5" ~ "5"
  ),
  Area = case_when(
    str_sub(Well, 1, 1) == "P" ~ "Well",
    str_sub(Well, 1, 1) == "C" ~ "Creek"
  ), Date = mdy_hms(Date)) %>% 
  filter(SPC_before < 1000, year(Date) > 2024)


dates <- as_date(manual_well_creek$Date)
df_full <- df_full %>% 
  mutate(matches = case_when(
    date %in% dates ~ "sampled",
    .default = "not sampled"))


df1 = df_full %>% 
  filter(matches == "sampled", hour(dt_round) <= 12, hour(dt_round) >= 9) %>% 
  select(date, `Specific Conductivity`) %>% 
  mutate(avg = mean(`Specific Conductivity`))
