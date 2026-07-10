### Instructions:
# 1. Make sure there are no files in folder 'dtw_upload' and that the only file in 'dtw_formatted' is 'P1_baro_formatted'. 
# 2. Drop new XLE files into folder 'dtw_upload' and give them the following names as appropriate: "P1_baro", "P1", "Bridge", "P5"
# 3. Run all uncommented code. This will format the xle files and drop them into 'dtw_formatted' as .csvs.
# 4. To check graphs of water level at the bridge, P1, and P5, run the extra code at the bottom.

# This will need to be adjusted to transform water height to height above sea level. I'll wait to do this until we've surveyed Bell Filed etc



## Install dependencies etc

# install.packages("lubridate")
# install.packages("xml2")
# install.packages("XML")
# install.packages("pracma")

library(tidyverse)
library(lubridate)
library(xml2)
library(XML)
library(pracma)



## Data cleaning

# This function does 2 things: 
# a. Run a Hampel filter on the dataframe to remove outliers (i.e. slice out all measurements taken when the probe was removed from the well for sampling)
# b. Correct an ongoing issue with depth loggers being placed in different places in the water column, mostly due to tangles in the cable and getting jammed in the well.
# It goes along the entire dataframe row by row, identifies segments with a sudden drop that couldn't have been caused by changes in well level, then calculates the magnitude
# of that drop and applies that value of correction to all impacted measurements. This assumes that the normal values are the correct water level in the well, and that there is
# no difference between the measurement before and after the drop occurred. Unfortunately this chunk of code is pretty inefficient and can take a little while to run.

CleanData = function(df){
  data = df$height_above
  result = hampel(data, k = 5, t0 = 3)
  df_hampel = df %>% 
    filter(height_above > 0.003) %>% 
    slice(-result$ind)
  
 # Much of the rest of this function came from Copilot but functions correctly 
  correction <- 0       
  in_block <- FALSE      
  
  for (i in 2:length(df_hampel$height_above)) {
    
    # compute difference using original values
    diff_val <- df_hampel$height_above[i - 1] - df_hampel$height_above[i]
    
    # detect start of a new erroneous block
    if (diff_val > 0.2 && !in_block) { # This is the best threshold for incorrect logger placement I've found for now -- basically, if there's a jump in water level of more than 
                                       # 0.2 meters, the loop goes into error correction mode and starts applying a correction to all following erroneous datapoints.
                                       # Adjust if it starts correcting drops caused by actual change in water level. Maybe adjust to percent change?
      correction <- diff_val   # lock in the correction for this block
      in_block <- TRUE
    }
    
    # detect end of erroneous block (data realigns)
    if (diff_val <= 0.2 && in_block) { 
      correction <- 0
      in_block <- FALSE
    }
    # apply correction if inside a block
    df_hampel$height_above[i] <- df_hampel$height_above[i] + correction
  }
  df_hampel %>% data.frame()
}



## Formatting XLE files
# This function handles the weird Solinst file type as XMLs. It reads in the different components of the file as individual vectors, cleans them up, stitches them into a dataframe, and writes that DF to a .csv. 

handle_xle = function(location, elevation = NULL) {
  
  items_to_pull = c("Date", "Time", "ch1", "ch2") # Set up XML nodes
  result = c("Date", "Time", "Pressure", "Temperature") 
  cols_of_interest = vector("list", length(items_to_pull))
  names(cols_of_interest) = result # Set up column names for final dataframe
 
  if (location != "P1_baro") { # only do this if it's a levelogger and not a barologger
   p1_baro = read_csv("dtw/dtw_formatted/P1_baro_formatted.csv")
  }
  
  
  path = paste0("dtw/dtw_upload/", location, ".xle")
  xle_file = read_xml(path) # Read in file
  
  for (i in seq_along(items_to_pull)) {
    cols_of_interest[[i]] = xml_text(xml_find_all(xle_file, paste0(".//", items_to_pull[i]))) # pull actual numbers out of XML. cols_of_interest becomes a vector containing 4 lists of values. 
  }
  
  for (i in (1:2)) { # Some files apparently have an extra date/time with no data attached. Not sure why this happens. This should catch that issue. Sorry for the terrible indexing.
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
    mutate(datetime_rounded = round_date(datetime, unit = "15 mins")) %>% # Round time to align with barometric pressure reading
    left_join(p1_baro %>% select(datetime, Pressure), by = join_by(datetime_rounded == datetime)) %>% # attach pressure data
    mutate(baro_pressure = (Pressure.y * 0.101972), height_above = (Pressure.x - baro_pressure)) %>%  # Convert atmospheric pressure to feet then subtract off
    drop_na() %>% 
    relocate(datetime_rounded) # Clean up
   
  df = CleanData(df) # Run Hampel filter and adjust for potential misplacement of probe in the water column
   
  if (!is.null(elevation)){
    df = df %>% 
      mutate(water_elevation = height_above + elevation) # Transform height of water above sensor 
  }
   
 }

 write_csv(df, paste0("dtw/dtw_formatted/", location, "_formatted.csv"))
}



## Set up list of sites and handle files

sites = c("Bridge", "P1", "P5", "P1_baro")
elevations = c(273.2, 273.3, 274.3, NA)
results = Map(handle_xle, sites, elevations) # This is the line that iterates over the leveloggers to clean up the xle files. It takes the 4 sites and 3 elevation correction factors used 
                                             # to transform height above sensor to elevation of water. N.B. there are only 3 elevations because the P1 barologger doesn't take one 
                                             # and the function is setup to handle the missing value. 



## Check on data
# Run all below lines and run to look at time series of water level

check_csv = function(location) { # Use this to make graphs of temperature over time. Intended to check that dates/expected data gaps are behaving as they ought.
  if (location != "P1_baro") {
  file = read_csv(paste0("dtw/dtw_formatted/", location, "_formatted.csv"))
  p = file %>%
    ggplot(aes(x = datetime, y = water_elevation)) +
    geom_line() +
    labs(title = location)
  print(p)
  }
}

for (site in sites) {
  assign(site, read_csv(paste0("dtw/dtw_formatted/", site, "_formatted.csv")))
  check_csv(site)
}

