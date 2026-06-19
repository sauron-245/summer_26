####################################################################################
##### Instructions: #####                                                          #
# 1. Download all new data from HOBO loggers as .xlsx files                        #
# 2. Drop files into folder 'wells_upload'                                         #
# 3. Rename all files using the following conventions:                             #
#     a. piezometers: well name alone (e.g. 'P3')                                  #
#     b. creek temp monitors: well name plus '_creek' (e.g. P5_creek)              # 
#     c. creek conductivity monitors: same as temp plus _cond (e.g. P5_creek_cond) #
# 4. In body of script, add all filenames in 'wells_upload' to the 'wells_week'    #  
#    vector in quotation marks (e.g. wells_week = c("P1", "P3", "P5_creek"))       #
# 5. Run all lines of code. If you encounter errors, I suggest calling the         #
#    update_well_log() function on each file in order to determine which one is    # 
#    causing issues. It may be that one or more files are already up to date.      # 
# 6. Once code has run without issue, delete all files in 'wells_upload'.          #
##### Good luck! #####                                                             #    
####################################################################################

# install.packages("tidyverse")
# install.packages("lubridate") # Comment in if packages have not been installed

library(tidyverse)
library(lubridate)
library(readxl)


update_well_log = function(site) {
  cols_to_check = c("Date-Time", "Temperature", "Electrical Conductivity", "Specific Conductivity", "Salinity", "Total Dissolved Solids")
  log_standing <- read_csv(paste0("wells_running/", site, "_current.csv")) # Running total of log data
  
  log_new <- read_excel(paste0("wells_upload/", site, ".xlsx")) %>%  # Most recently downloaded data
    rename_with(~ trimws(sub("\\(.*$", "", .x))) %>% # Correctly format column names and get rid of metadata columns
    select(any_of(cols_to_check))
  
  
  
  date_current_last = as_datetime(log_standing[[nrow(log_standing),1]])
  date_new = as_datetime(log_new[[nrow(log_new),1]])

  if (date_current_last == date_new) { # Checks if running log data are already up to date
    stop(paste0("Most recent date of running log data for site ", site, " matches last date on added data. Have you already merged these dataframes?"))
  } 

  data_to_add = log_new %>%
    filter(`Date-Time` > date_current_last) # selects only data not present in running logs

  log_standing_updated = rbind(log_standing, data_to_add)
  write_csv(log_standing_updated, paste0("wells_running/", site, "_current.csv")) # Adds new data and overwrites existing. csv
}

wells_week = c("P1_creek")
for (well in wells_week) {
  update_well_log(well)
}

p2_new = read_excel("P4.xlsx")%>%  # Most recently downloaded data
  rename_with(~ trimws(sub("\\(.*$", "", .x))) %>%   # Correctly format column names and get rid of 1st column
  select(-1) %>% 
  rename(Temperature = `Temperature , °C`)
#   mutate(
#     Date.Time..CDT. = format(
#       as.POSIXct(Date.Time..CDT., format = "%m.%d.%Y %H:%M:%S ", tz = "UTC"),
#       "%Y-%m-%d %H:%M:%S"
#     )
  ) %>%
  rename(`Date-Time` = Date.Time..CDT.) %>%
  mutate(`Date-Time` = as_datetime(`Date-Time`)) %>%
  rename(Temperature = Temperature.....C.)

p4_running = read.csv("wells_running/P4_creek_current.csv") %>% 
  mutate(
    Date.Time = format(
      as.POSIXct(Date.Time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      "%Y-%m-%d %H:%M:%S"
    )
  ) %>%
  rename(`Date-Time` = Date.Time)%>%
  mutate(`Date-Time` = as_datetime(`Date-Time`)) %>%
  rename(Temperature = Temperature....C)
P2_running = rbind(p2_new, p4_running)
