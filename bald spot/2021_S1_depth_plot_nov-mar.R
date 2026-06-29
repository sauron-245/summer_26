library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(ggplot2)


# connects to the dts postgres database (read-only)
connect_dts <- function() {
  dbConnect(RPostgres::Postgres(),
            dbname = "dts_db", host = "dts.physics.carleton.edu",
            port = "5432", user = "dts_user_ro",
            password = "$$N0th1ng5p3c14l$$")
}


# pulls every channel-1 observation on the 15th of the listed months in 2022,
# already carrying real depth straight from the database
get_15th_2022 <- function() {
  con <- connect_dts()
  on.exit(dbDisconnect(con), add = TRUE)
  
  q <- "
    SELECT M.id AS measurement_id, M.datetime_utc,
           D.laf_m, D.depth_m, D.temperature_c
    FROM dts_data D
    INNER JOIN measurement M ON M.id = D.measurement_id
    INNER JOIN channel H ON M.channel_id = H.id
    WHERE H.channel_name = 'channel 1'
      AND H.fiber_topology_name = 'baldspot'
      AND date_part('year',  M.datetime_utc) = 2021
      AND date_part('day',   M.datetime_utc) = 15
      AND date_part('month', M.datetime_utc) IN (1, 2, 3, 11, 12)
    ORDER BY M.datetime_utc, D.laf_m;"
  
  dbGetQuery(con, q) %>% as_tibble()
}


# keeps the measurement nearest noon on each of those days, clips to the S1 laf
# window, and adds month/time labels for plotting
prep_s1 <- function(df, target_hour = 12) {
  df %>%
    mutate(datetime = as_datetime(datetime_utc, tz = "UTC"),
           date_only = as_date(datetime),
           hour = hour(datetime), minute = minute(datetime),
           mins_from_target = abs((hour * 60 + minute) - target_hour * 60)) %>%
    group_by(date_only) %>%
    filter(measurement_id == measurement_id[which.min(mins_from_target)]) %>%
    ungroup() %>%
    filter(laf_m > 230.3, laf_m < 388.8) %>%
    mutate(month_label = factor(format(datetime, "%Y-%m")))
}


# --- run it ---

raw  <- get_15th_2022()
s1   <- prep_s1(raw)

# how many days actually came back (some 15ths may be missing)
s1 %>% distinct(date_only)

# plots s1 downhole temperature for each month's 15th, colored by month
ggplot(s1, aes(temperature_c, depth_m, group = measurement_id, color = month_label)) +
  geom_path(linewidth = 0.6) +
  scale_y_reverse() +
  xlim(0, 25) +
  labs(x = "Temperature (°C)", y = "Depth (m)", color = "month",
       title = "S1 — 15th of each month, 2022 (Nov–Mar)") +
  theme_bw()


