library(tsibble)
library(imputeTS)
library(feasts)
library(tidyverse)
library(openintro)
library(GGally)
#install.packages("corrr")
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
#install.packages("pracma")
library(pracma)
library(prophet)

setwd("~/Documents/Data-Mining/Series_Temporales/TP-Series-Temporales//")





############################
#AGRUPO UN REGISTRO POR DIA#
############################



df <- read.csv("AirQualityUCI.csv")

gases_filtro <- c("PT08.S1.CO.","C6H6.GT.","PT08.S2.NMHC.","PT08.S3.NOx.","PT08.S4.NO2.","PT08.S5.O3.","T","RH","AH" )


#Pongo los -200 como nulos
df[df == -200] <- NA


#Elimino filas nulas
df$DateTime <-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
df <- df[which(is.na(df$DateTime) ==FALSE),]

#Hago imputacion de media movil
df2 <- df
for(i in gases_filtro){
  df2[paste0(i,'_imp')]<- na_ma(df2[i], k = 7, weighting = "exponential")
}

#1 Registro por dia tomando la media
gases_filtro
df_g <- df2 %>%  group_by(Date) %>%  summarise (across(everything(), mean))

df_g$Date <- as.Date(as.character(df_g$Date),format="%Y-%m-%d")

gases_filtro_imp <- paste0(gases_filtro,'_imp')


df_g$ds <- df_g$Date
df_g$y <- df_g$T_imp

train <- df_g[df_g$Date < "2005-03-01",]
test <- df_g[df_g$Date >= "2005-03-01",]


m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=20)
m<-fit.prophet(m,train)


future <- make_future_dataframe(m, periods = 35)

forecast <- predict(m,future)

plot(m, forecast)


prediccion = append(train[356,]$T_imp,forecast$yhat[357:391])
test$prediccion <- prediccion[-1]



ggplot() + geom_line(data=df_g, aes(x=Date,y=T_imp)) + geom_line(data=test,aes(x=Date,y=prediccion),color='blue')



forecast$yhat