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

  
  path = paste0(location, ".xle")
  xle_file = read_xml(path) # Read in file
  
  for (i in seq_along(items_to_pull)) {
    cols_of_interest[[i]] = xml_text(xml_find_all(xle_file, paste0(".//", items_to_pull[i]))) # pull actuall numbers out of XML. cols_of_interest becomes a vector containing 4 lists of values. 
  }
  for (i in (1:2)) { # Some files apparently have an extra date/time with no data attached. Not sure why this happens. This should catch that issue. 
    if (length(cols_of_interest[[i]]) != length(cols_of_interest[[4]])) {
      cols_of_interest[[i]] = cols_of_interest[[i]][2:(length(cols_of_interest[[i]]))]
    }
  }
 df = as.data.frame(cols_of_interest, stringsAsFactors = FALSE) %>% 
   mutate(Date = ymd(Date)) # Turn vector of lists into dataframe 
 return(df) # Change this 
}
handle_xle("P5_8_31")

