## Install dependencies etc

# install.packages("xml2")
# install.packages("XML")
library(tidyverse)
library(lubridate)
library(xml2)
library(XML)


## Function perhaps?

handle_xle = function(location) {
  
  items_to_pull = c("Date", "Time", "ch1", "ch2") # Set up XML nodes
  result = c("Date", "Time", "Pressure", "Temperature") 
  cols_of_interest = vector("list", length(items_to_pull))
  names(cols_of_interest) = result # Set up column names for final dataframe

 
  if (location != "P1_baro") { # only do this if it's a levelogger and not a barologger
   p1_baro = read_csv("P1_baro_formatted.csv") # 
  }
  
  path = paste0(location, ".xle")
  xle_file = read_xml(path) # Read in file
  
  for (i in seq_along(items_to_pull)) {
    cols_of_interest[[i]] = xml_text(xml_find_all(xle_file, paste0(".//", items_to_pull[i]))) # pull actual numbers out of XML. cols_of_interest becomes a vector containing 4 lists of values. 
  }
  for (i in (1:2)) { # Some files apparently have an extra date/time with no data attached. Not sure why this happens. This should catch that issue. 
    if (length(cols_of_interest[[i]]) != length(cols_of_interest[[4]])) {
      cols_of_interest[[i]] = cols_of_interest[[i]][2:(length(cols_of_interest[[i]]))]
    }
  }
  df = as.data.frame(cols_of_interest, stringsAsFactors = FALSE) %>% # Turn vector of lists into dataframe 
    mutate(Date = ymd(Date), datetime = paste(Date, Time, sep = " "), datetime = ymd_hms(datetime), Temperature = as.numeric(Temperature), Pressure = as.numeric(Pressure)) %>% # merge 'date' and 'time' columns into formatted datetime
    select(c(-1, -2)) %>% 
    relocate(datetime) # Clean up
 if (location != "P1_baro"){ # only do this if it's a levelogger and not a barologger
   df = df %>%
    mutate(datetime = round_date(datetime, unit = "15 mins")) %>% # Round time to align with barometric pressure reading
    left_join(p1_baro %>% select(datetime, Pressure), by = "datetime") %>% # attach pressure data
    mutate(baro_pressure = (Pressure.y * 0.101972), height_above = (Pressure.x - baro_pressure)) # Convert atmospheric pressure to feet then subtract off
 }
 write_csv(df, paste0(location, "_formatted.csv"))
 
}


sites = c("P1_baro", "P1", "Bridge", "P5")

for (site in sites) {
  handle_xle(site)
}

for (site in sites) {
  assign(site, read_csv(paste0(site, "_formatted.csv")))
}

check_csv = function(location) { # Use this to make graphs of temperature over time. Intended to check that dates/expected data gaps are behaving as they ought. 
  if (location != "P1_baro"){
  file = read_csv(paste0(location, "_formatted.csv")) 
  p = file %>% 
    ggplot(aes(x = datetime, y = height_above)) +
    geom_line() + 
    labs(title = location)
  print(p)
  }
}
for (site in sites){
  check_csv(site)
}

bridge_correct = read_csv("BridgeLevellogger.6.23.26.csv") %>% 
  rename(datetime = DateTime) %>% 
  mutate(datetime = mdy_hm(datetime)) %>% 
  rename(height_above = LEVEL)
ggplot(Bridge, aes(x = datetime, y = height_above)) +
  geom_line()+
  geom_line(data = bridge_correct)
