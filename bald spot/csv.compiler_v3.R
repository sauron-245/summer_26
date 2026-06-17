### function to collect DTS data csv files and put them into a workable dataframe 
setwd("~/Documents/Projects/Geothermal/geothermal - DTS Data - Feb 2022 Update")

csv.compiler <- function(start.year = 2019,
                         end.year = 2023, 
                         start.month = 01, 
                         end.month = 12, 
                         start.day = 01, 
                         end.day = 01,
                         start.hour = 00,
                         end.hour = 00){
  
  all.files <- list.files(recursive = TRUE) ## this reads all data from subdirectories 
  all.files <- as_tibble(all.files) %>% mutate(file.path.length = nchar(value))
  
  temp.file.list <- all.files %>% mutate(
    channel = substr(value, 1,9),
    year = as.numeric(substr(value, (file.path.length-20),(file.path.length-17))),
    month = as.numeric(substr(value, (file.path.length-16),(file.path.length-15))),
    day = as.numeric(substr(value, (file.path.length-14),(file.path.length-13))), 
    hour = as.numeric(substr(value,(file.path.length-12),(file.path.length-11))),
    minutes = as.numeric(substr(value, (file.path.length-10),(file.path.length-9))),
    file.type = substr(value, (file.path.length-3),(file.path.length-0))
  )
  
  selected.files <- temp.file.list %>% filter(
    year >= start.year & year <= end.year &
      month >= start.month & month <= end.month &
      day >= start.day & day <= end.day & hour >=start.hour & hour <= end.hour &
      file.type == ".csv"
  )
  
  file.list.to.read <- as.list(selected.files$value)
  
  compiled.data.list <- tibble(filename = file.list.to.read) %>% # create a data frame
    # holding the file names
    mutate(file_contents = map(filename,          # read files into
                               ~ read_csv(file.path(.))) # a new data column
    ) 
  
  compiled.data.df <- unnest(compiled.data.list, cols = file_contents)
  
  compiled.data.df.final <- compiled.data.df %>% mutate(
    file.path.length = nchar(filename),
    channel = substr(filename, 1,9),
    year = as.numeric(substr(filename, (file.path.length-20),(file.path.length-17))),
    month = as.numeric(substr(filename, (file.path.length-16),(file.path.length-15))),
    day = as.numeric(substr(filename, (file.path.length-14),(file.path.length-13))), 
    hour = as.numeric(substr(filename,(file.path.length-12),(file.path.length-11))),
    minutes = as.numeric(substr(filename, (file.path.length-10),(file.path.length-9))),
    seconds = as.numeric(substr(filename, (file.path.length-8),(file.path.length-7))),
    full.datetime = paste(year, month, day, hour, minutes, sep = ""),
    datetime = ymd_hms(paste(year,"-", month,"-",day,"T",hour,
                             ":",minutes,":",seconds, sep=""))
  )
  
  colnames(compiled.data.df.final) <- c("filename","length_along_fiber_m",
                                        "temp_C","elevation_WGS84_m",
                                        "depth_m","slope","file.path.length","channel","year","month",
                                        "day","hour","minutes","seconds",
                                        "full.datetime","datetime")
  compiled.data.df.final
}