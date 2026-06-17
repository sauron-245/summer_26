

## attempt at replicating figure 2 from Fandel et al. 
daily_examples %>%
  ggplot(aes(x = temperature_c, y = depth_m,
             color = season, linetype = system_status)) +
  geom_path() +
  scale_y_reverse() +
  theme_bw() +
  labs(x = "Temperature (°C)", y = "Depth (m)",
       color = "Season", linetype = "System status")

