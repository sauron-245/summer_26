

library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

#Connection
con <- dbConnect(RPostgres::Postgres(),
                 dbname   = "dts_db",
                 host     = "dts.physics.carleton.edu",
                 port     = 5432,
                 user     = "dts_user_ro",
                 password = "$$N0th1ng5p3c14l$$")
########################################################
#Reusable pull
########################################################

get_dts_day <- function(con, year, month, day) {
  q <- sprintf("
    SELECT H.channel_name,
           date_part('year',   M.datetime_utc) AS year,
           date_part('month',  M.datetime_utc) AS month,
           date_part('day',    M.datetime_utc) AS day,
           date_part('hour',   M.datetime_utc) AS hour,
           date_part('minute', M.datetime_utc) AS minute,
           D.laf_m, D.depth_m, D.temperature_c
    FROM dts_data AS D
    INNER JOIN measurement AS M ON M.id = D.measurement_id
    INNER JOIN channel AS H ON M.channel_id = H.id
    WHERE M.channel_id IN (SELECT id FROM channel WHERE fiber_topology_name = 'baldspot')
      AND date_part('year',  M.datetime_utc) = %d
      AND date_part('month', M.datetime_utc) = %d
      AND date_part('day',   M.datetime_utc) = %d
    ORDER BY H.id, D.laf_m;", year, month, day)
  
  dbGetQuery(con, q) %>% mutate(
    borehole = case_when(
      channel_name == "channel 1" & laf_m > 230.3  & laf_m < 388.8  ~ "S1",
      channel_name == "channel 1" & laf_m > 775.1  & laf_m < 933.6  ~ "S2",
      channel_name == "channel 1" & laf_m > 1255.9 & laf_m < 1424.4 ~ "S3",
      channel_name == "channel 3" & laf_m > 562.0  & laf_m < 730.5  ~ "S4",
      channel_name == "channel 3" & laf_m > 123.0  & laf_m < 291.5  ~ "S5"
    )
  ) %>% filter(!is.na(borehole))
}
########################################################
#FIG 2a Pre- vs syn-operation, winter & summer, S1
########################################################

summer_syn <- get_dts_day(con, 2022, 8, 25)   # syn-operation summer
winter_syn <- get_dts_day(con, 2020, 1, 7)    # syn-operation winter
summer_pre <- get_dts_day(con, 2019, 7, 15)   # pre-operation summer
winter_pre <- get_dts_day(con, 2019, 3, 15)   # pre-operation winter

fig2a_data <- bind_rows(
  summer_syn %>% mutate(season = "summer", system_status = "syn-operation"),
  winter_syn %>% mutate(season = "winter", system_status = "syn-operation"),
  summer_pre %>% mutate(season = "summer", system_status = "pre-operation"),
  winter_pre %>% mutate(season = "winter", system_status = "pre-operation")
) %>%
  filter(borehole == "S1") %>%
  # paper uses a single representative hour (11am-noon local ~ 17-18 UTC)
  filter(hour == 17)

fig2a <- ggplot(fig2a_data,
                aes(temperature_c, depth_m,
                    color = season, linetype = system_status)) +
  geom_path() +
  scale_y_reverse() +
  scale_color_manual(values = c(winter = "lightblue", summer = "darkorange")) +
  theme_bw() +
  labs(x = "Temperature (°C)", y = "Depth (m)",
       color = "Season", linetype = "System status",
       title = "Fig 2A — S1 pre- vs syn-operation")
print(fig2a)

########################################################
#FIG 2C Hourly profiles over a single day comparing winter & summer
########################################################
fig2b_data <- bind_rows(
  winter_syn %>% mutate(season = "winter"),
  summer_syn %>% mutate(season = "summer")
) %>% filter(borehole == "S1")

fig2b <- ggplot(fig2b_data,
                aes(temperature_c, depth_m, color = hour, group = hour)) +
  geom_path() +
  scale_y_reverse() +
  scale_color_magma_c() +
  theme_bw() +
  facet_wrap(~ season) +
  labs(x = "Temperature (°C)", y = "Depth (m)", color = "Hour (UTC)",
       title = "Fig 2B — S1 hourly profiles")
print(fig2b)

########################################################
#FIG 2C monthly profiles 
########################################################
months_15 <- lapply(1:12, function(m) {
  out <- tryCatch(get_dts_day(con, 2021, m, 15), error = function(e) NULL)
  out
})
fig2c_data <- bind_rows(months_15) %>%
  filter(borehole == "S1", hour == 17)   # ~noon local snapshot

fig2c <- ggplot(fig2c_data,
                aes(temperature_c, depth_m, color = factor(month), group = month)) +
  geom_path() +
  scale_y_reverse() +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Temperature (°C)", y = "Depth (m)", color = "Month",
       title = "Fig 2C — S1 monthly profiles (2021, 15th of each month)")
print(fig2c)



########################################################
#Fig 3 
#too big of a query to run
########################################################
fig3_raw <- dbGetQuery(con, "
  SELECT H.channel_name,
         date(M.datetime_utc) AS day,
         D.laf_m,
         D.temperature_c
  FROM dts_data AS D
  INNER JOIN measurement AS M ON M.id = D.measurement_id
  INNER JOIN channel AS H ON M.channel_id = H.id
  WHERE M.channel_id IN (SELECT id FROM channel WHERE fiber_topology_name = 'baldspot')
  ORDER BY day;")

fig3_data <- dbGetQuery(con, "
  SELECT borehole,
         day,
         AVG(temperature_c) AS mean_temp
  FROM (
    SELECT date(M.datetime_utc) AS day,
           D.temperature_c,
           CASE
             WHEN H.channel_name = 'channel 1' AND D.laf_m > 230.3  AND D.laf_m < 388.8  THEN 'S1'
             WHEN H.channel_name = 'channel 1' AND D.laf_m > 775.1  AND D.laf_m < 933.6  THEN 'S2'
             WHEN H.channel_name = 'channel 1' AND D.laf_m > 1255.9 AND D.laf_m < 1424.4 THEN 'S3'
             WHEN H.channel_name = 'channel 3' AND D.laf_m > 562.0  AND D.laf_m < 730.5  THEN 'S4'
             WHEN H.channel_name = 'channel 3' AND D.laf_m > 123.0  AND D.laf_m < 291.5  THEN 'S5'
           END AS borehole
    FROM dts_data AS D
    INNER JOIN measurement AS M ON M.id = D.measurement_id
    INNER JOIN channel AS H ON M.channel_id = H.id
    WHERE M.channel_id IN (SELECT id FROM channel WHERE fiber_topology_name = 'baldspot')
  ) sub
  WHERE borehole IS NOT NULL
  GROUP BY borehole, day
  ORDER BY day;")
#fig3_data$day <- as.Date(fig3_data$day)

#fig3 <- ggplot(fig3_data, aes(day, mean_temp, color = borehole)) +
  geom_line() +
  theme_bw() +
  labs(x = "Date", y = "Mean daily temperature (°C)", color = "Borehole",
       title = "Fig 3 — Mean daily temperature per borehole")
#print(fig3)

