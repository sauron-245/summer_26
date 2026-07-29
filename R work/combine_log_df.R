##### Instructions: #####                                                          
# 1. Download all new data from HOBO loggers as .xlsx files                        
# 2. Drop files into folder 'hobos/hobos_upload'                                         
# 3. Rename all files using the following conventions:                             
#     a. piezometers: hobo name alone (e.g. 'P3')                                  
#     b. creek temp monitors: hobo name plus '_creek' (e.g. P5_creek)              
#     c. creek conductivity monitors: same as temp plus _cond (e.g. P5_creek_cond) 
#     d. Downstream monitor: 'Dwnstrm'; Spring: 'Spring'; Midstream: 'Mdstrm'                                            
# 4. In body of script, add all filenames in 'hobos/hobos_upload' to the 'hobos_week'      
#    vector in quotation marks (e.g. hobos_week = c("P1", "P3", "P5_creek")). If you're
#    updating data for all monitors, change both for loops at the bottom of the script to 
#    iterate over 'hobos_all' instead of 'hobos_week'.
# 5. Source the script. If you encounter errors, I suggest calling             
#    update_hobo_log() on each file in order to determine which one is             
#    causing issues. The code is designed to catch several likely issues, and      
#    may provide information on which site is causing problems in the terminal.     
# 6. Once code has run without issue, delete all files in 'hobos/hobos_upload'.          


## Comment in if packages have not been installed: 
# install.packages("tidyverse")
# install.packages("lubridate") 
# install.packages("readxl")

setwd("~/GitHub/summer_26/R work")

library(tidyverse)
library(lubridate)
library(readxl)

hobos_all = c("Dwnstrm", "P1", "P1_creek", "P2", "P2_creek", "P3", "Mdstrm", "Spring", "P4", "P4_creek", "P5", "P5_creek", "P5_creek_cond") # Default option for all HOBO monitors. 

update_hobo_log = function(site) {
  cols_to_check = c("Date-Time", "Temperature", "Temperature , °C", "Electrical Conductivity", "Specific Conductivity", "Salinity", "Total Dissolved Solids")
  
  log_standing <- read_csv(paste0("hobos/hobos_running/", site, "_current.csv")) %>% # Running total of log data
    mutate(`Date-Time` = as.POSIXct(`Date-Time`))
  log_new <- read_excel(paste0("hobos/hobos_upload/", site, ".xlsx")) %>%  # Most recently downloaded data
    rename_with(~ trimws(sub("\\(.*$", "", .x))) %>% # Correctly format column names and get rid of metadata columns
    select(any_of(cols_to_check)) %>%  # References list of columns of interest and selects just columns containing real data, removing metadata/empty columns
    mutate(`Date-Time` = as.POSIXct(`Date-Time`))
  
  if (length(names(log_new)) != 6) {
    if (length(names(log_new)) != 2) {
    stop(paste0("Check column names/format at site ", site, ".")) # This will hopefully catch issues caused by mismatched column names between the running .csv and newly uploaded data
    }
  }
  
  if ("Temperature , °C" %in% names(log_new)){
    log_new = log_new %>% rename(Temperature = `Temperature , °C`) # Some of the hobos use a different column name for temperature 
  }
  
  date_current_last = as_datetime(log_standing[[nrow(log_standing),1]]) # Last datapoint present in running data log
  date_new = as_datetime(log_new[[nrow(log_new),1]]) # Last datapoint present in new data

  if (date_current_last == date_new) { # Checks if running log data are already up to date
    stop(paste0("Most recent date of running log data for site ", site, " matches last date on added data. Have you already merged these dataframes?"))
  } 

  data_to_add = log_new %>%
    filter(`Date-Time` > date_current_last) %>% # selects only data not present in running logs
    filter(Temperature > 0) # Catches some points collected when the monitor was removed from the well during winter
  log_standing_updated = rbind(log_standing, data_to_add) # Attach new data
  log_standing_updated = log_standing_updated %>% 
    mutate(`Date-Time` = format(`Date-Time`, "%Y-%m-%d %H:%M:%S")) # Ensure date-time is correctly formatted
  rm(list = c("data_to_add", "log_standing", "log_new")) # Clean up temp files
  
  write_csv(log_standing_updated, paste0("hobos/hobos_running/", site, "_current.csv")) 
}

hobos_week = c("P5_creek_cond") # Default option for all HOBO monitors. 

for (hobo in hobos_all) {
  update_hobo_log(hobo)
}

## By default, the above for loop updates all running hobo data. In order to target updates to specific files, (for instance, if you missed a hobo in the field),
## replace 'hobos_all' with a vector containing just the sites of interest (e.g. c("P1", "P3", "P5_creek")).

check_csv = function(site) { # Use this to make graphs of temperature over time. Intended to check that dates/expected data gaps are behaving as they ought. 
  file = read_csv(paste0("hobos/hobos_running/", site, "_current.csv")) 
  p = file %>% 
  ggplot(aes(x = `Date-Time`, y = Temperature)) +
    geom_line() + 
    labs(title = paste0("Temperature at ", site, " from ", min(file$`Date-Time`), " to ", max(file$`Date-Time`)))
  print(p)
  }

for (hobo in hobos_all) {
  check_csv(hobo)
}

