library(tsibble)
#install.packages("feasts")
#install.packages("imputeTS")
library(imputeTS)
library(feasts)
library(tidyverse)
library(openintro)
library(GGally)
library(corrr)
library(knitr)
library(tidymodels)
library(gridExtra)
library(rsample)
library(ggplot2)
library(GGally)
library(RColorBrewer)
library(ggcorrplot)
library(forcats)
library(robustbase)
library(fpp3)
library(tseries)
library(slider)
library(forecast)
library(lubridate)

setwd("~/Documents/Data-Mining/Series_Temporales/TP/")

df <- read.csv("AirQualityUCI.csv")


df$DateTime <-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")


df <- df[which(is.na(df$DateTime) ==FALSE),]


df <- df  %>% as_tsibble(index= DateTime)

gases <- c("CO.GT.","PT08.S1.CO.","NMHC.GT.","C6H6.GT.","PT08.S2.NMHC.","NOx.GT.","PT08.S3.NOx.","NO2.GT.",
           "PT08.S4.NO2.","PT08.S5.O3.","T","RH","AH" )

gases_filtro <- c("PT08.S1.CO.","C6H6.GT.","PT08.S2.NMHC.","PT08.S3.NOx.","PT08.S4.NO2.","PT08.S5.O3.","T","RH","AH" )

df2[df2 == -200] <- NA

sapply(df2, function(x){sum(is.na(x)/dim(df2)[1])})


autoplot(df2, PT08.S1.CO. ) +
  labs(title = "CO.GT.", y = "CO.GT.", x = "DateTime")


for(x in gases_filtro){
  autoplot(df2,x  ) +
    labs(title = x, y = x, x = "DateTime")
}