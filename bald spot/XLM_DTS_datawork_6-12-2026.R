setwd("~/Downloads")

#install.packages("xml2")
library(xml2)

d <- read_xml("channel 1_UTC_20260410_193607.846.xml")

xml_structure(d)
xml_name(d)
xlm_children(d)

## strip namespace 
d2 <- xml_ns_strip(d)

data_nodes <- xml_find_all(d2, "//data")
length(data_nodes)
