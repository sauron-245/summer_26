############################
# To use:
# 1. drop .xlsx files of interest into 'wells_merge' folder
# 2. in last line of code, rename "df1" and "df2" to the names of your .xlsx files
# 3. Within function definition, adjust date formatting to ensure read_excel treats
#    dates as appropriate. This was a major issue in developing this script -- dates are 
#    formatted inconsistently between HOBO sensors. 
# 4. Run all code. Merged data will be written to a .csv located in the 'wells_merge' folder.
###########################


# install.packages("tidyverse")
# install.packages("lubridate") # Comment in if packages have not been installed

library(tidyverse)
library(lubridate)
library(readxl)


combine_df = function(xl1, xl2){
  df1 <- read_excel(paste0("wells_merge/", xl1, ".xlsx"),
                    range = cell_cols("A:G"),
                    col_names = c("obs", "Date-Time", "Temp", "Conductivity", "SPC", "Salinity", "TDS"),
                    col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% 
    slice(-1)  %>% # Removes headers, which are transformed into a row after specifying col_names.
    mutate(
      `Date-Time` = parse_date_time(`Date-Time`,
        orders = c("mdy HMS", "mdy HM", "ymd HMS", "ymd HM"))) 
  # Date formatting should be addressed on a dataframe-by-dataframe basis
    

  df2 <- read_excel(paste0("wells_merge/", xl2, ".xlsx"),
                    range = cell_cols("A:G"),
                    col_names = c("obs", "Date-Time", "Temp", "Conductivity", "SPC", "Salinity", "TDS"),
                    col_types = c("numeric", "date", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% 
    slice(-1)  
    

  
  if (ncol(df1) != ncol(df2)) {
    stop("Column counts do not match — cannot rbind.")
  }
  
  df_new <- rbind(df1, df2)
  
}
P1 = combine_df("P1_1", "P1_2") # Adjust names as needed.
#  May produce warnings -- these are likely due to inconsistencies in column format that we addressed by slicing out the 1st row of each dataframe.

write.csv(P1, file = "wells_merge/P1_all.csv")
