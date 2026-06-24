# install.packages("xml2")
# install.packages("XML")
library(tidyverse)
library(lubridate)
library(xml2)
library(XML)

testdata = read_xml("level_test.xle")
test = xmlParse(testdata)
xml_structure(testdata)
dates = xml_text(xml_find_all(testdata, ".//Date"))
times = xml_text(xml_find_all(testdata, ".//Time"))
pressures = xml_text(xml_find_all(testdata, ".//ch1"))
temps = xml_text(xml_find_all(testdata, ".//ch2"))
dates = dates[2:4046]
times = times[2:4046]
test_df = tibble(Date = dates, Time = times, Pressure = pressures, Temp = temps) %>% 
  data.frame()


# Function perhaps?

handle_xle = function(location) {
  items_to_pull = c("Date", "Time", "ch1", "ch2")
  cols_of_interest = c("Date", "Time", "Pressure", "Temperature")
  
  path = paste0(location, ".xle")
  xle_file = read_xml(path)
  
  for (node in items_to_pull) {
    cols_of_interest[as.integer(node)] = xml_text(xml_find_all(xle_file, paste0(".//", node))
  }
}