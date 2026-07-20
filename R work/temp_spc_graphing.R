library(tidyverse)
library(lubridate)
library(pracma)

for (site in hobos_all) {
  assign(site, read_csv(paste0("hobos/hobos_running/", site, "_current.csv")))
}
dfs = list()
hobos_all = c("Dwnstrm", "P1", "P1_creek", "P2", "P2_creek", "P3","P4_creek",  "Mdstrm", "Spring", "P4", "P5", "P5_creek", "P5_creek_cond") 
for (site in hobos_all) {
  data = read_csv(paste0("hobos/hobos_running/", site, "_current.csv"))
  data = data %>% drop_na()
  to_filter = data$'Specific Conductivity'
  result = hampel(to_filter, k = 100, t0 = 2)
  final = data %>% slice(-result$ind)
  
  
  dfs[[site]] = final[, c("Date-Time", "Specific Conductivity", "Temperature")]

}
df_full = bind_rows(dfs, .id = "Site") %>% drop_na()


P1 = df_full %>%
  filter(Site == "P1") %>% 
  ggplot() + 
  geom_line(aes(x = `Date-Time`, y = Temperature), color = 'red') + 
  geom_line(aes(x = `Date-Time`, y = `Specific Conductivity`/75), color = 'blue') +
  scale_y_continuous(sec.axis = sec_axis(~. * 75)) + 
  theme_bw() +
  labs(title = "SPC & Temperature at P1")
print(P1)

P2 = df_full %>%
  filter(Site == "P2") %>% 
  ggplot() + 
  geom_line(aes(x = `Date-Time`, y = Temperature), color = 'red') + 
  geom_line(aes(x = `Date-Time`, y = `Specific Conductivity`/75), color = 'blue') +
  scale_y_continuous(sec.axis = sec_axis(~. * 75)) + 
  theme_bw() +
  labs(title = "SPC & Temperature at P2")
print(P2)

p3 = df_full %>% 
  filter(!(Site %in% c("Dwnstrm", "P3_creek_cond", "P5_creek_cond", "Spring"))) %>% 
  ggplot(aes(x = `Date-Time`, y = `Specific Conductivity`, color = Site)) +
  geom_line() +
  theme_bw()
print(p3)

p4 = df_full %>% 
  filter(Site %in% c("P5_creek_cond", "P5")) %>%
  ggplot(aes(x = `Date-Time`, y = `Specific Conductivity`, color = Site))+
  # geom_line(data = df_full %>% filter(Site == "P5"), aes(x = `Date-Time`, y = `Specific Conductivity`)) +
  geom_line() +
  # geom_smooth(data = df_full %>% filter(Site == "P5_creek_cond"), aes(x = `Date-Time`, y = `Specific Conductivity`), method = "loess", se = FALSE, span = .05) +
  theme_bw()
print(p4)

p5 = df_full %>% 
  filter(Site %in% c("Dwnstrm", "P5_creek_cond")) %>% 
  ggplot(aes(x = `Date-Time`, y = `Temperature`, color = Site)) +
  # geom_line(linewidth = 0.5) +
  geom_smooth(method = "loess", se = FALSE, span = 0.01) +
  theme_bw(base_size = 20)
print(p5)  

p6 = 
  ggplot() +
  geom_line(data = Dwnstrm %>% filter(year(`Date-Time`) == 2026), aes(x = `Date-Time`, y = `Specific Conductivity`), linewidth = 0.5) +
  geom_line(data = P2 %>% filter(year(`Date-Time`) == 2026), aes(x = `Date-Time`, y = `Specific Conductivity`), linewidth = 0.5) +
  geom_line(data = P5_creek_cond %>% filter(year(`Date-Time`) == 2026), aes(x = `Date-Time`, y = `Specific Conductivity`), linewidth = 0.5) +
  geom_smooth(method = "loess", se = FALSE, span = 0.01) +
  theme_bw()
print(p6)  


ggplot() + 
  geom_line(data = P5, aes(x = `Date-Time`, y = Temperature, color = "Well")) + 
  geom_line(data = P5_creek, aes(x = `Date-Time`, y = Temperature, color = "Creek"))
  