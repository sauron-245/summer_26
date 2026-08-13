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
    Nitrate = as.numeric(Chloride), Flouride = as.numeric(Flouride), Chloride = as.numeric(Chloride)
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
  ggplot(aes(x = Date_full, y = Chloride)) +
  geom_point() +
  facet_grid(~PumpTest_Creek)
print(plot)
plot1 = anions %>%
  select(-c(Phosphate, Bromide, Nitrite)) %>% 
  filter(Well == site, SampleNum == 1) %>% 
  group_by(Date_full, PumpTest_Creek) %>% 
  mutate(avg = mean(Chloride)) %>% 
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
  group_by(Well, Date) %>%
  summarize(avg = mean(Flouride)) %>%
  ggplot(aes(x = Date, y = avg, color = Well)) +
  geom_point() + 
  # geom_line() +
  # scale_color_manual(values = pal2) +
  theme_bw()


plot2 = anions %>%
  select(-c(Phosphate, Bromide, Nitrite)) %>% 
  filter(PumpTest_Creek != "C") %>% 
  group_by(Date_full, Well) %>% 
  mutate(avg = mean(Chloride)) %>% 
  ggplot(aes(x = Date_full, y = avg, color = Well)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title = "Nitrate time series, averaged by well for each date", y = "Averaged Nitrate Concentration", x = "Date")
print(plot2)


plot3 = 
  ggplot() + 
  geom_point(data = anions %>% 
               filter(PumpTest_Creek != "C", Well == "P1") %>% 
               group_by(Date_full) %>% 
               summarize(avg = mean(Chloride, na.rm = TRUE), .groups = "drop"),
             aes(x = Date_full, y = avg, color = "P1 Well")) + 
  geom_line(data = anions %>% 
              filter(PumpTest_Creek != "C", Well == "P1") %>% 
              group_by(Date_full) %>% 
              summarize(avg = mean(Chloride, na.rm = TRUE), .groups = "drop"),
            aes(x = Date_full, y = avg), color = 'lightblue') +
  geom_point(data = anions %>% 
               filter(PumpTest_Creek == "C", Well == "P1") %>% 
               group_by(Date_full) %>% 
               summarize(avg = mean(Chloride, na.rm = TRUE), .groups = "drop"),
             aes(x = Date_full, y = avg, color = "P1 Creek")) + 
  geom_line(data = anions %>% 
              filter(PumpTest_Creek == "C", Well == "P1") %>% 
              group_by(Date_full) %>% 
              summarize(avg = mean(Chloride, na.rm = TRUE), .groups = "drop"),
            aes(x = Date_full, y = avg), color = 'red') +
  theme_bw() + 
  labs(title = "Nitrate at P1, well vs. Creek", color = "Location", x = "Date", y = "Nitrate Concentration")
print(plot3)

terrible_plot = function(df, site, metric) {
  plot = 
    ggplot() + 
    geom_point(data = df %>% 
                 filter(PumpTest_Creek != "C", Well == site) %>% 
                 group_by(Date_full) %>% 
                 summarize(avg = mean(.data[[metric]], na.rm = TRUE), .groups = "drop"),
               aes(x = Date_full, y = avg, color = paste0(site, " well"))) + 
    geom_line(data = df %>% 
                filter(PumpTest_Creek != "C", Well == site) %>% 
                group_by(Date_full) %>% 
                summarize(avg = mean(.data[[metric]], na.rm = TRUE), .groups = "drop"),
              aes(x = Date_full, y = avg), color = 'lightblue') +
    geom_point(data = df %>% 
                 filter(PumpTest_Creek == "C", Well == site) %>% 
                 group_by(Date_full) %>% 
                 summarize(avg = mean(.data[[metric]], na.rm = TRUE), .groups = "drop"),
               aes(x = Date_full, y = avg, color = paste0(site, " creek"))) + 
    geom_line(data = df %>% 
                filter(PumpTest_Creek == "C", Well == site) %>% 
                group_by(Date_full) %>% 
                summarize(avg = mean(.data[[metric]], na.rm = TRUE), .groups = "drop"),
              aes(x = Date_full, y = avg), color = 'red') +
    theme_bw(base_size = 20) + 
    scale_y_continuous(limits = c(0, 80)) +
    labs(title = paste0(metric, " at ", site, " well vs. Creek"), color = "Location", x = "Date", y = paste0(metric, " Concentration"))
  return(plot)
}

terrible_plot("P1", "Sulfate")
terrible_plot("P3", "Sulfate")
terrible_plot("P5", "Sulfate")
terrible_plot("P1", "Nitrate")
terrible_plot("P3", "Nitrate")
terrible_plot("P5", "Nitrate")
terrible_plot("P1", "Chloride")

## dan 
aa <- terrible_plot("P1", "Sulfate")
ab <- terrible_plot("P3", "Sulfate")
ac <- terrible_plot("P5", "Sulfate")
ba <- terrible_plot("P1", "Nitrate")
bb <- terrible_plot("P3", "Nitrate")
bc <- terrible_plot("P5", "Nitrate")
ca <- terrible_plot("P1", "Chloride")
cb <- terrible_plot("P3", "Chloride")
cc <- terrible_plot("P5", "Chloride") 

install.packages("gridExtra")
library(gridExtra)

(aa | ab |ac)/(ba |bb | bc)/(ca | cb |cc)+  plot_layout(guides = 'collect')

anions %>% filter(PumpTest_Creek != "A" & Well != "DO") %>% arrange(Date_full) %>%
  group_by(Date_full, Well) %>% 
  ggplot(aes(Date_full, Chloride, fill = PumpTest_Creek)) + geom_path() + 
  geom_point(pch = 21, size = 3) + facet_wrap(vars(Well)) + theme_bw(base_size = 16)


anions %>% 
  filter(Date_full == "2025-11-18") %>% 
  group_by(Well, PumpTest_Creek) %>% 
  summarize(avg = mean(Nitrate)) %>% 
  ggplot()

cations %>% filter(PumpTest_Creek != "A" & Well != "DO") %>% arrange(Date) %>%
  group_by(Date, Well) %>% 
  ggplot(aes(Date, Magnesium, fill = PumpTest_Creek)) + geom_path() + 
  geom_point(pch = 21, size = 3) + facet_wrap(vars(Well)) + theme_bw(base_size = 16)


site = "P1"
df = anions
metric = "Nitrate"
plot = df %>% 
  filter(PumpTest_Creek != "A", Well == site) %>% 
  group_by(Date_full, PumpTest_Creek) %>% 
  summarize(avg = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = Date_full, y = avg, fill = PumpTest_Creek)) + 
    geom_point() + 
    geom_path()
print(plot)

plot = df %>% 
  filter(PumpTest_Creek != "A", Well == site)
