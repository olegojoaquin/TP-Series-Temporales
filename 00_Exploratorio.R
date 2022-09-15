library(readr)
df <-  read_delim("Documents/Data-Mining/Series_Temporales/AirQualityUCI/AirQualityUCI.csv", 
                  delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                                       Time = col_time(format = "%H.%M.%S")), 
                  locale = locale(decimal_mark = ",", grouping_mark = ""), 
                  trim_ws = TRUE)
library(lubridate)
library(tidyverse)
head(df)
df$DateTime <-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")


plot(df$DateTime,df$AH)

write_csv(df,"Documents/Data-Mining/Series_Temporales/TP/AirQualityUCI.csv")