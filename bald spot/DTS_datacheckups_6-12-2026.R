# -----------
# Load or install required packages 
# ------------

# install.packages('RPostgres') only run if needed (remove # to activate code line)

library(RPostgres)
library(dplyr)
library(ggplot2)
library(lubridate)

# ----------


##### 
## we are following the example on this blog: https://www.datacareer.de/blog/connect-to-postgresql-with-r-a-step-by-step-example/
library(DBI) ## not sure what this is...

db <- 'dts_db'  #name of database
host_db <- 'dts.physics.carleton.edu' #host name  
db_port <- '5432'  #port number
db_user <- 'dts_user_ro'  #user name (provided by Bruce Duffy...do not share)
db_password <- '$$N0th1ng5p3c14l$$' # password (provided by Bruce Duffy...do not share)

con <- dbConnect(RPostgres::Postgres(), ### establishes a connection with database
                 dbname = db, 
                 host=host_db, 
                 port=db_port, 
                 user=db_user, 
                 password=db_password)  

## check status of connection
dbListTables(con) 

####
daily_test <- dbGetQuery(con, "SELECT D.measurement_id, 
H.channel_name, 
H.id AS channel_id, 
date_part('year', M.datetime_utc) AS year, 
date_part('month', M.datetime_utc) AS month, 
date_part('day', M.datetime_utc) AS day, 
date_part('hour', M.datetime_utc) as hour, 
date_part('minute', M.datetime_utc) as minute,
D.slope, 
D.laf_m, 
D.elevation_m, 
D.depth_m, 
D.temperature_c
FROM dts_data AS D 
INNER JOIN measurement AS M ON M.id = D.measurement_id 
INNER JOIN channel AS H ON M.channel_id = H.id 
WHERE M.channel_id IN (SELECT id from channel WHERE channel.fiber_topology_name = 'baldspot')
AND date_part('year', M.datetime_utc) = 2022
AND date_part('month', M.datetime_utc) = 08
AND date_part('day', M.datetime_utc) = 25
ORDER BY H.id, D.laf_m;")

#### daily example split 
## split to differentiate boreholes
daily_test_c1 <- filter(daily_test, channel_name == "channel 1")
daily_test_c3 <- filter(daily_test, channel_name == "channel 3")
## boreholes for channel 1
daily_test_c1 <- daily_test_c1 %>% mutate(borehole = case_when(
  laf_m >230.3 & laf_m < 388.8 ~ 'S1',
  laf_m >775.1 & laf_m < 933.6 ~ 'S2',
  laf_m >1255.9 & laf_m < 1424.4 ~ 'S3'
))
# boreholes for channel 3
daily_test_c3 <- daily_test_c3 %>% mutate(borehole = case_when(
  laf_m >562.0 & laf_m < 730.5 ~ 'S4',
  laf_m >123.0 & laf_m < 291.5 ~ 'S5'
))
# recombine to create full data set 
daily_example_Sept21_2020 <- rbind(daily_test_c1, daily_test_c3)

daily_example_Sept21_2020 %>% filter(!is.na(borehole)) %>% mutate(
  datetime_str = paste(year, month, day, hour, minute, sep = "-"),
  datetime = ymd_hm(datetime_str)) %>% 
  group_by(datetime) %>%
  ggplot(aes(temperature_c, depth_m, color = as.factor(hour))) + geom_path() + 
  scale_y_reverse() + 
  theme_bw() + 
  facet_grid(year ~ borehole) 

daily_example_Sept21_2020 %>% filter(!is.na(borehole)) %>% mutate(
  datetime_str = paste(year, month, day, hour, minute, sep = "-"),
  datetime = ymd_hm(datetime_str)) %>% 
  group_by(month, depth_m, borehole) %>% summarize(
    temp_diff = max(temperature_c) - min(temperature_c)
  ) %>%
  ggplot(aes(temp_diff, depth_m)) + geom_path() + 
  scale_y_reverse() + 
  theme_bw() + facet_grid(cols = vars(borehole))

##### Winter test 
####
daily_test2 <- dbGetQuery(con, "SELECT D.measurement_id, 
H.channel_name, 
H.id AS channel_id, 
date_part('year', M.datetime_utc) AS year, 
date_part('month', M.datetime_utc) AS month, 
date_part('day', M.datetime_utc) AS day, 
date_part('hour', M.datetime_utc) as hour, 
date_part('minute', M.datetime_utc) as minute,
D.slope, 
D.laf_m, 
D.elevation_m, 
D.depth_m, 
D.temperature_c
FROM dts_data AS D 
INNER JOIN measurement AS M ON M.id = D.measurement_id 
INNER JOIN channel AS H ON M.channel_id = H.id 
WHERE M.channel_id IN (SELECT id from channel WHERE channel.fiber_topology_name = 'baldspot')
AND date_part('year', M.datetime_utc) = 2020
AND date_part('month', M.datetime_utc) = 01
AND date_part('day', M.datetime_utc) = 7 
ORDER BY H.id, D.laf_m;")

#### daily example split 
## split to differentiate boreholes
daily_test2_c1 <- filter(daily_test2, channel_name == "channel 1")
daily_test2_c3 <- filter(daily_test2, channel_name == "channel 3")
## boreholes for channel 1
daily_test2_c1 <- daily_test2_c1 %>% mutate(borehole = case_when(
  laf_m >230.3 & laf_m < 388.8 ~ 'S1',
  laf_m >775.1 & laf_m < 933.6 ~ 'S2',
  laf_m >1255.9 & laf_m < 1424.4 ~ 'S3'
))
# boreholes for channel 3
daily_test2_c3 <- daily_test2_c3 %>% mutate(borehole = case_when(
  laf_m >562.0 & laf_m < 730.5 ~ 'S4',
  laf_m >123.0 & laf_m < 291.5 ~ 'S5'
))
# recombine to create full data set 
daily_example_Jan7_2020 <- rbind(daily_test2_c1, daily_test2_c3)

daily_example_Jan7_2020 %>% filter(!is.na(borehole) & borehole == "S1") %>% mutate(
  datetime_str = paste(year, month, day, hour, minute, sep = "-"),
  datetime = ymd_hm(datetime_str)) %>% 
  group_by(datetime) %>%
  ggplot(aes(temperature_c, depth_m, color = as.factor(hour))) + geom_path() + 
  scale_y_reverse() + 
  theme_bw() + 
  facet_grid(year ~ borehole, scales="free", space="free_x") 

daily_example_Jan7_2020 %>% filter(!is.na(borehole)) %>% mutate(
  datetime_str = paste(year, month, day, hour, minute, sep = "-"),
  datetime = ymd_hm(datetime_str)) %>% 
  group_by(month, depth_m, borehole) %>% summarize(
    temp_diff = max(temperature_c) - min(temperature_c)
  ) %>%
  ggplot(aes(temp_diff, depth_m)) + geom_path() + 
  scale_y_reverse() + 
  theme_bw() + facet_grid(cols = vars(borehole))

## grab pre-operation data from 2019
daily_test3 <- dbGetQuery(con, "SELECT D.measurement_id, 
H.channel_name, 
H.id AS channel_id, 
date_part('year', M.datetime_utc) AS year, 
date_part('month', M.datetime_utc) AS month, 
date_part('day', M.datetime_utc) AS day, 
date_part('hour', M.datetime_utc) as hour, 
date_part('minute', M.datetime_utc) as minute,
D.slope, 
D.laf_m, 
D.elevation_m, 
D.depth_m, 
D.temperature_c
FROM dts_data AS D 
INNER JOIN measurement AS M ON M.id = D.measurement_id 
INNER JOIN channel AS H ON M.channel_id = H.id 
WHERE M.channel_id IN (SELECT id from channel WHERE channel.fiber_topology_name = 'baldspot')
AND date_part('year', M.datetime_utc) = 2019
AND date_part('month', M.datetime_utc) = 7
AND date_part('day', M.datetime_utc) = 15 
ORDER BY H.id, D.laf_m;")

#### daily example split 
## split to differentiate boreholes
daily_test3_c1 <- filter(daily_test3, channel_name == "channel 1")
daily_test3_c3 <- filter(daily_test3, channel_name == "channel 3")
## boreholes for channel 1
daily_test3_c1 <- daily_test3_c1 %>% mutate(borehole = case_when(
  laf_m >230.3 & laf_m < 388.8 ~ 'S1',
  laf_m >775.1 & laf_m < 933.6 ~ 'S2',
  laf_m >1255.9 & laf_m < 1424.4 ~ 'S3'
))
# boreholes for channel 3
daily_test3_c3 <- daily_test3_c3 %>% mutate(borehole = case_when(
  laf_m >562.0 & laf_m < 730.5 ~ 'S4',
  laf_m >123.0 & laf_m < 291.5 ~ 'S5'
))
# recombine to create full data set 
daily_example_July15_2019 <- rbind(daily_test3_c1, daily_test3_c3)

## check data
daily_example_July15_2019 %>% filter(!is.na(borehole)) %>% mutate(
  datetime_str = paste(year, month, day, hour, minute, sep = "-"),
  datetime = ymd_hm(datetime_str)) %>% 
  group_by(datetime) %>% filter(borehole == "S1") %>%
  ggplot(aes(temperature_c, depth_m, color = as.factor(hour))) + geom_path() + 
  scale_y_reverse() + 
  theme_bw() + 
  facet_grid(year ~ borehole, scales="free", space="free_x") 

## grab pre-operation data from 2019
daily_test4 <- dbGetQuery(con, "SELECT D.measurement_id, 
H.channel_name, 
H.id AS channel_id, 
date_part('year', M.datetime_utc) AS year, 
date_part('month', M.datetime_utc) AS month, 
date_part('day', M.datetime_utc) AS day, 
date_part('hour', M.datetime_utc) as hour, 
date_part('minute', M.datetime_utc) as minute,
D.slope, 
D.laf_m, 
D.elevation_m, 
D.depth_m, 
D.temperature_c
FROM dts_data AS D 
INNER JOIN measurement AS M ON M.id = D.measurement_id 
INNER JOIN channel AS H ON M.channel_id = H.id 
WHERE M.channel_id IN (SELECT id from channel WHERE channel.fiber_topology_name = 'baldspot')
AND date_part('year', M.datetime_utc) = 2019
AND date_part('month', M.datetime_utc) = 3
AND date_part('day', M.datetime_utc) = 15
ORDER BY H.id, D.laf_m;")

#### daily example split 
## split to differentiate boreholes
daily_test4_c1 <- filter(daily_test4, channel_name == "channel 1")
daily_test4_c3 <- filter(daily_test4, channel_name == "channel 3")
## boreholes for channel 1
daily_test4_c1 <- daily_test4_c1 %>% mutate(borehole = case_when(
  laf_m >230.3 & laf_m < 388.8 ~ 'S1',
  laf_m >775.1 & laf_m < 933.6 ~ 'S2',
  laf_m >1255.9 & laf_m < 1424.4 ~ 'S3'
))
# boreholes for channel 3
daily_test4_c3 <- daily_test4_c3 %>% mutate(borehole = case_when(
  laf_m >562.0 & laf_m < 730.5 ~ 'S4',
  laf_m >123.0 & laf_m < 291.5 ~ 'S5'
))
# recombine to create full data set 
daily_example_March15_2019 <- rbind(daily_test4_c1, daily_test4_c3)

## check data
daily_example_March15_2019 %>% filter(!is.na(borehole)) %>% mutate(
  datetime_str = paste(year, month, day, hour, minute, sep = "-"),
  datetime = ymd_hm(datetime_str)) %>% 
  group_by(datetime) %>% filter(borehole == "S1") %>%
  ggplot(aes(temperature_c, depth_m, color = as.factor(hour))) + geom_path() + 
  scale_y_reverse() + 
  theme_bw() + 
  facet_grid(year ~ borehole, scales="free", space="free_x") 



### data to share for S1 only - for SERC activity
S1_Sept212020_example <- daily_example_Sept21_2020 %>% filter(borehole == "S1")
S1_Jan72020_example <- daily_example_Jan7_2020 %>% filter(borehole == "S1")
S1_July152019_example <- daily_example_July15_2019 %>% filter(borehole == "S1")
S1_March152019_example <- daily_example_March15_2019 %>% filter(borehole == "S1")

daily_examples <- rbind(S1_Sept212020_example, S1_Jan72020_example, S1_July152019_example, S1_March152019_example)

daily_examples <- daily_examples %>% select(year, month, day, hour, minute, depth_m, temperature_c)

daily_examples <- daily_examples %>% mutate(
  str_datetime = paste(month,"-",day,"-",year," ",hour,":",minute,":","00", sep = ""),
  datetime_UTC = mdy_hms(str_datetime),
  datetime_CST = with_tz(datetime_UTC, tzone = "America/Chicago"),
  hour_CST = as.numeric(hour(datetime_CST)),
  season = case_when(month == "3" ~ "winter",
                     month == "1" ~ "winter",
                     month == "7" ~ "summer",
                     month == "9" ~ "summer"),
  system_status = case_when(year == "2019" ~ "pre-operation",
                            year >= "2020" ~ "syn-operation")
) 

head(daily_examples)

write.csv(daily_examples, "Daily_examples_Oct172025.csv")

Daily_examples %>% filter(as.factor(hour) == "6") %>% ggplot(aes(temperature_c, depth_m, color = as.factor(hour))) + geom_path() + scale_y_reverse()
