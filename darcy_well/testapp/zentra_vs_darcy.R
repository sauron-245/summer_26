############################################################
#  Darcy Well — water level vs geoloop pumping
#
#  Loads and cleans the HYDROS 21 water-level logger, aligns it with the
#  minute-cadence geoloop pump data, and tests whether water level responds
#  to pumping rate. Also sets up EC and water temp as mixing tracers.
############################################################

library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(readxl)


# --- 1. load + clean the water level logger --------------------------------
#  3 header rows; timestamps are 12-hour AM/PM; keep the columns we need.
wl_raw <- read_csv(
  "zentra.csv",
  skip = 3, col_names = FALSE, show_col_types = FALSE)

water <- wl_raw %>%
  transmute(
    TIMESTAMP = mdy_hms(X1, tz = "UTC"),
    level_mm  = as.numeric(X2),
    water_temp_C = as.numeric(X3),
    EC = as.numeric(X4)
  ) %>%
  filter(!is.na(TIMESTAMP), !is.na(level_mm)) %>%
  mutate(level_ft = level_mm / 304.8)

cat("water level:", nrow(water), "rows,",
    format(min(water$TIMESTAMP)), "to", format(max(water$TIMESTAMP)), "\n")


# --- 2. load the geoloop data (from the other script) ----------------------
geo <- read_excel("P829_Geoloop_63026_72726.xlsx", sheet = "Export") %>%
  transmute(TIMESTAMP = as_datetime(TIMESTAMP, tz = "UTC"),
            gpm = GeoloopGPM, vfd = VFDFreq,
            from_bldg_F = GeoloopFromBldgT, to_bldg_F = GeoloopToBldgT)


# --- 3. align them on a common 15-min grid ---------------------------------
#  water level is 15-min; average the 1-min geoloop into 15-min bins, then join.
geo_15 <- geo %>%
  mutate(t15 = floor_date(TIMESTAMP, "15 minutes")) %>%
  group_by(t15) %>%
  summarize(gpm = mean(gpm, na.rm = TRUE),
            vfd = mean(vfd, na.rm = TRUE),
            from_bldg_F = mean(from_bldg_F, na.rm = TRUE),
            to_bldg_F = mean(to_bldg_F, na.rm = TRUE),
            .groups = "drop")

joined <- water %>%
  mutate(t15 = floor_date(TIMESTAMP, "15 minutes")) %>%
  left_join(geo_15, by = "t15") %>%
  filter(!is.na(gpm))

# =========================================================
#  BETTER-CORRELATION EXPLORATION
# =========================================================

library(dplyr)

# make sure it's time-ordered before any lag/difference operations
joined <- joined %>% arrange(TIMESTAMP)

# --- 1. rate of change of level (drawdown rate) ---
joined <- joined %>%
  mutate(level_dt = level_ft - lag(level_ft))

cat("level vs gpm (raw):    ", round(cor(joined$level_ft, joined$gpm, use="complete.obs"), 3), "\n")
cat("level_dt vs gpm:       ", round(cor(joined$level_dt, joined$gpm, use="complete.obs"), 3), "\n")

# --- 2. pump-on only ---
on <- joined %>% filter(vfd > 0)
cat("level_dt vs gpm (on):  ", round(cor(on$level_dt, on$gpm, use="complete.obs"), 3), "\n")

# --- 3. lag: which offset best aligns pumping with level ---
cc <- ccf(joined$gpm, joined$level_ft, lag.max = 20,
          na.action = na.omit, plot = TRUE)
best <- cc$lag[which.max(abs(cc$acf))]
cat("best lag (15-min steps):", best, "\n")

# --- 4. water level vs pumping over time (dual view) -----------------------
#  scale gpm to overlay on the level axis for visual comparison
lvl_rng <- range(joined$level_ft, na.rm = TRUE)
gpm_rng <- range(joined$gpm, na.rm = TRUE)
scale_gpm <- function(x) (x - gpm_rng[1])/diff(gpm_rng) * diff(lvl_rng) + lvl_rng[1]

ggplot(joined, aes(TIMESTAMP)) +
  geom_line(aes(y = level_ft, color = "water level (ft)"), linewidth = 0.4) +
  geom_line(aes(y = scale_gpm(gpm), color = "pump GPM (scaled)"),
            linewidth = 0.3, alpha = 0.7) +
  scale_color_manual(values = c("water level (ft)" = "blue",
                                "pump GPM (scaled)" = "red")) +
  labs(x = NULL, y = "Water level (ft)", color = NULL,
       title = "Water level vs pumping rate over time") +
  theme_bw()


# --- 5. direct relationship: level vs gpm ----------------------------------
ggplot(joined, aes(gpm, level_ft)) +
  geom_point(alpha = 0.3, size = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Pump flow (GPM)", y = "Water level (ft)",
       title = "Does higher pumping draw the water level down?") +
  theme_bw()

# correlation (expect negative: more pumping -> lower level)
cat("correlation level vs gpm:",
    round(cor(joined$level_ft, joined$gpm, use = "complete.obs"), 3), "\n")


# --- 6. EC and water temp as mixing tracers --------------------------------
#  EC and temperature can reveal whether pumped/injected water is mixing
#  between aquifers. watch for shifts that track pumping.
ggplot(joined, aes(TIMESTAMP)) +
  geom_line(aes(y = EC), color = "darkgreen", linewidth = 0.3) +
  labs(x = NULL, y = "EC (mS/cm)",
       title = "Electrical conductivity over time (mixing tracer)") +
  theme_bw()