library(tidyverse)
library(lubridate)
library(RColorBrewer)

cations = read_csv("Creek and Well Cations - Sheet1.csv")

cations = cations %>% 
  mutate(
    MeasurementDate = mdy(MeasurementDate), 
    Date = mdy(Date),
    Calcium = as.numeric(Calcium), Magnesium = as.numeric(Magnesium), Potassium = as.numeric(Potassium), Sodium = as.numeric(Sodium) 
  )

anions = read_csv("Creek and Well Anion Data - newdatasorted.csv")
anions = anions %>% 
  mutate(
    MeasurementDate = mdy(MeasurementDate),
    Date_full = mdy(paste(Month, Day, Year, sep = " ")),
    Nitrate = as.numeric(Nitrate), Flouride = as.numeric(Flouride), Chloride = as.numeric(Chloride)
  )

for(num in seq_along(1:5)) {
  site = paste0("P", num)
  p = cations %>% 
    filter(str_detect(Well, site), PumpTest_Creek != "C") %>%
    # drop_na() %>% 
    ggplot(aes(x = Date, y = Calcium, color = PumpTest_Creek)) + 
    geom_point() +
    # geom_line() +
    theme_minimal() +
    labs(title = paste0("Calcium at site ", site))
  print(p)
}




num = 1
site = paste0("P", num)
plot = anions %>%
  select(-c(Phosphate, Bromide, Nitrite)) %>% 
  drop_na() %>% 
  filter(Well == site) %>% 
  # group_by(PumpTest_Creek) %>%
  ggplot(aes(x = Date_full, y = Nitrate)) +
  geom_point() +
  facet_grid(~PumpTest_Creek)
print(plot)
plot1 = anions %>%
  select(-c(Phosphate, Bromide, Nitrite)) %>% 
  filter(Well == site, PumpTest_Creek != "C", SampleNum == 1) %>% 
  group_by(Date_full, PumpTest_Creek) %>% 
  mutate(avg = mean(Nitrate)) %>% 
  ggplot(aes(x = Date_full, y = avg, color = PumpTest_Creek)) +
  geom_point() +
  geom_line()
print(plot1)  

data = anions %>%
  select(-c(Phosphate, Bromide, Nitrite)) %>% 
  drop_na() %>% 
  filter(Well == site, PumpTest_Creek != "C", SampleNum == 1) %>% 
  group_by(Date_full)


# pal2 = brewer.pal(n = 4, "Set2")[1:10]
cations %>% 
  filter(str_detect(Well, "P1"), PumpTest_Creek != "C", Dilution == "1:1") %>% 
  ggplot(aes(x = Date, y = Potassium, color = PumpTest_Creek)) + 
  geom_point()

anions %>% 
  filter(!is.na(SampleNum), PumpTest_Creek != "C") %>%
  drop_na() %>% 
  # group_by(Well, Date) %>% 
  # summarize(avg = mean(Flouride)) %>% 
  ggplot(aes(x = Date, y = Flouride, color = Well)) +
  geom_point() + 
  # geom_line() +
  # scale_color_manual(values = pal2) +
  theme_bw()
