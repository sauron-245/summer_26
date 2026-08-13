window_day        <- "2026-07-23" 
picks <- pick_files()
picks %>% mutate(date = as_date(datetime)) %>% distinct(date) %>% arrange(date) %>% print(n = 50)

# fetch and add depth
dat <- fetch_and_parse(picks) %>% add_depth()


d_225 <- dat %>% filter(depth_m == 68.632)
d_175 <- dat  %>% filter(depth_m == 53.380)
d_74 <- dat %>% filter(depth_m == 22.622)
d_125 <- dat %>% filter(depth_m == 38.128)
parish_0723 = rbind(d_74, d_125, d_175, d_225)

window_day        <- "2026-07-24" 
picks <- pick_files()
picks %>% mutate(date = as_date(datetime)) %>% distinct(date) %>% arrange(date) %>% print(n = 50)

# fetch and add depth
dat <- fetch_and_parse(picks) %>% add_depth()


d_225 <- dat %>% filter(depth_m == 68.632)
d_175 <- dat  %>% filter(depth_m == 53.380)
d_74 <- dat %>% filter(depth_m == 22.622)
d_125 <- dat %>% filter(depth_m == 38.128)
parish_0724 = rbind(d_74, d_125, d_175, d_225)

window_day        <- "2026-07-25" 
picks <- pick_files()
picks %>% mutate(date = as_date(datetime)) %>% distinct(date) %>% arrange(date) %>% print(n = 50)

# fetch and add depth
dat <- fetch_and_parse(picks) %>% add_depth()


d_225 <- dat %>% filter(depth_m == 68.632)
d_175 <- dat  %>% filter(depth_m == 53.380)
d_74 <- dat %>% filter(depth_m == 22.622)
d_125 <- dat %>% filter(depth_m == 38.128)
parish_0725 = rbind(d_74, d_125, d_175, d_225)

window_day        <- "2026-07-08" 
picks <- pick_files()
picks %>% mutate(date = as_date(datetime)) %>% distinct(date) %>% arrange(date) %>% print(n = 50)

# fetch and add depth
dat <- fetch_and_parse(picks) %>% add_depth()


d_225 <- dat %>% filter(depth_m == 68.632)
d_175 <- dat  %>% filter(depth_m == 53.380)
d_74 <- dat %>% filter(depth_m == 22.622)
d_125 <- dat %>% filter(depth_m == 38.128)
parish_0708 = rbind(d_74, d_125, d_175, d_225)

window_day        <- "2026-07-07" 
picks <- pick_files()
picks %>% mutate(date = as_date(datetime)) %>% distinct(date) %>% arrange(date) %>% print(n = 50)

# fetch and add depth
dat <- fetch_and_parse(picks) %>% add_depth()


d_225 <- dat %>% filter(depth_m == 68.632)
d_175 <- dat  %>% filter(depth_m == 53.380)
d_74 <- dat %>% filter(depth_m == 22.622)
d_125 <- dat %>% filter(depth_m == 38.128)
parish_0707 = rbind(d_74, d_125, d_175, d_225)

window_day        <- "2026-07-06" 
picks <- pick_files()
picks %>% mutate(date = as_date(datetime)) %>% distinct(date) %>% arrange(date) %>% print(n = 50)

# fetch and add depth
dat <- fetch_and_parse(picks) %>% add_depth()


d_225 <- dat %>% filter(depth_m == 68.632)
d_175 <- dat  %>% filter(depth_m == 53.380)
d_74 <- dat %>% filter(depth_m == 22.622)
d_125 <- dat %>% filter(depth_m == 38.128)
parish_0706 = rbind(d_74, d_125, d_175, d_225)

parish_all <- rbind(parish_0706, parish_0707, parish_0708, parish_0723, parish_0724, parish_0725)
