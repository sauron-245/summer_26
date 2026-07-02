library(tidyverse)
library(lubridate)
library(pracma)
library(scico)
library(RColorBrewer)

sites = c("Dwnstrm", "P1", "P2", "P3", "P4", "P5", "P5_creek_cond") # Default option for all HOBO monitors. 
dfs = list()
level = read_csv("dtw/dtw_formatted/Bridge_formatted.csv")

manual = read_csv("manual_to_dl - Sheet1.csv") %>% drop_na() %>% rename(SPC = `SPC bottom before`) 
manual = manual %>% 
  mutate(Site = case_when(
    Location == "P1" ~ "1",
    Location == "P2" ~ "2",
    Location == "P3" ~ "3",
    Location == "P4" ~ "4",
    Location == "P5" ~ "5",
    Location == "C1" ~ "1",
    Location == "C2" ~ "2",
    Location == "C3" ~ "3",
    Location == "C4" ~ "4",
    Location == "C5" ~ "5"
  ), Date = mdy_hms(Date)) %>% 
  filter(SPC < 1000, year(Date) > 2025)

for (site in sites) {
  data = read_csv(paste0("hobos/hobos_running/", site, "_current.csv"))
  data = data %>% drop_na()
  to_filter = data$'Specific Conductivity'
  result = hampel(to_filter, k = 100, t0 = 2)
  final = data %>% slice(-result$ind)
  
  
  dfs[[site]] = final[, c("Date-Time", "Specific Conductivity")]
}

df_full = bind_rows(dfs, .id = "Site") %>% drop_na()

my_orange = brewer.pal(n = 7, "Oranges")[3:9]

df_mod = df_full %>% filter(Site != "P5_creek_cond", Site != "Dwnstrm")
P5 = df_full %>% filter(Site == "P5", `Date-Time` > "2026-02-03 13:45:00")

# P1: SPC at all piezometers without creek

p1 = ggplot(df_mod, aes(x = `Date-Time`, y = `Specific Conductivity`, color = Site))+ 
  geom_line() + 
  theme_minimal()

print(p1)

# P2: SPC at all piezometers with creek at P1 and P5

p2 = ggplot(df_full, aes(x = `Date-Time`, y = `Specific Conductivity`, color = Site))+ 
  geom_line() +
  theme_minimal()

print(p2)

# P3: manual data for 2026 at piezometers and corresponding creek locations

p3 = ggplot(manual, aes(x = Date, y = SPC, color = Site, shape = Area)) + 
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = my_orange) +
  theme_minimal()

print(p3)

# P4: spc at P5 vs. levellogger at bridge

p4 = level %>% 
  filter(datetime > "2024-11-21 11:47:50") %>% 
  ggplot(aes(x = datetime, y = height_above)) +
  geom_line() +
  geom_line(data = P5, aes(x = `Date-Time`, y = `Specific Conductivity` / 1000)) +
  scale_y_continuous(sec.axis = sec_axis(~. * 1000))
print(p4)   


df_full %>% 
  filter(`Date-Time` > "2026-01-01") %>% 
  ggplot(aes(x = `Date-Time`, y = `Specific Conductivity`, color = Site))+ 
  geom_line() +
  theme_minimal()


