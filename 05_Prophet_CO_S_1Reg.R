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

colnames(df)



#Pongo los -200 como nulos
df[df == -200] <- NA


#Elimino filas nulas
df$DateTime <-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
df <- df[which(is.na(df$DateTime) ==FALSE),]


# Changing the column names
dataset_columns = c('DATE', 'TIME','CO', 'CO_S', 'NMHC', 'NMHC_S', 'C6H6',
                    'NOX', 'NOX_S', 'NO2', 'NO2_S', 'O3_S','T', 'RH', 'AH',"DATETIME")

colnames(df) = dataset_columns


sapply(df, function(x){sum(is.na(x)/dim(df)[1])})


gases_filtro <- c("CO_S","NMHC_S","NOX_S","NO2_S","O3_S","T","RH","AH" )


#Hago imputacion de media movil
df2 <- df
for(i in gases_filtro){
  df2[paste0(i,'_IMP')]<- na_ma(df2[i], k = 7, weighting = "exponential")
}


###########################
#FILTRO POR MOVING AVERAGE#
###########################



df_movil <- df2
df_movil <- df_movil %>% mutate("CO_S_MA6" = movavg(df_movil$CO_S_IMP, 6,"s"))
df_movil <- df_movil %>% mutate("NMHC_S_MA6" = movavg(df_movil$NMHC_S_IMP, 6,"s"))
df_movil <- df_movil %>% mutate("NOX_S_MA6" = movavg(df_movil$NOX_S_IMP, 6,"s"))
df_movil <- df_movil %>% mutate("NO2_S_MA6" = movavg(df_movil$NO2_S_IMP, 6,"s"))
df_movil <- df_movil %>% mutate("O3_S_MA6" = movavg(df_movil$O3_S_IMP, 6,"s"))
df_movil <- df_movil %>% mutate("T_MA6" = movavg(df_movil$T_IMP, 6,"s"))
df_movil <- df_movil %>% mutate("RH_MA6" = movavg(df_movil$RH_IMP, 6,"s"))
df_movil <- df_movil %>% mutate("AH_MA6" = movavg(df_movil$AH_IMP, 6,"s"))

#AS TSIBBLE
df_movil <- df_movil  %>% as_tsibble(index= DATETIME)

df_movil %>% autoplot(CO_S_MA6, colour = 'black') +   labs( x= "DateTime") 



#SARIMA CO_S_MA6

#BOXCOX
min(df_movil$DATETIME)
max(df_movil$DATETIME)

train <- df_movil[df_movil$DATETIME< "2005-03-01 00:00:00 -03",]
test  <-  df_movil[df_movil$DATETIME >= "2005-03-15 00:00:00 -03",]


kpss.test(df_movil$CO_S_MA6)
adf.test(df_movil$CO_S_MA6,k = 0)


Lambda<- BoxCox.lambda(train$CO_S_MA6)

myts <- ts(train$CO_S_MA6)

#myts <- ts(df_movil[,c("DateTime","MA12")]$MA12, frequency=24L)
fit_am = auto.arima(myts, seasonal = T,parallel = TRUE, ic ="aic",stepwise=FALSE, trace=TRUE,lambda='auto')

#CHECK PREDICTIONS

plot(forecast(fit_am))
forecast_T <- forecast(fit_am,h=495,level=c(95))

predict_T<-predict(fit_am, n.ahead=495)

pred_con_ult_train = append(train[8526,]$CO_S_MA6,predict_T$pred)

test$prediccion <- pred_con_ult_train[-1]

ggplot() + geom_line(data=df_movil, aes(x=DATETIME,y=CO_S_MA6)) + geom_line(data=test,aes(x=DATETIME,y=prediccion),color='blue')



#PROPHET
df_movil$ds <- df_movil$DATETIME
df_movil$y <- df_movil$CO_S_MA6

train <- df_movil[df_movil$DATETIME< "2005-03-01 00:00:00 -03",]
test  <-  df_movil[df_movil$DATETIME >= "2005-03-15 00:00:00 -03",]

m <- prophet()
m <- add_seasonality(m, name ='yearly',period=8760,fourier.order=30)
m <- add_seasonality(m, name ='daily',period=24,fourier.order = 30)
m<-fit.prophet(m,train)


future <- make_future_dataframe(m, periods = 495)

forecast <- predict(m,future)

plot(m, forecast)


prediccion = append(train[8526,]$CO_S_MA6,forecast$yhat[8527:9021])
test$prediccion <- prediccion[-1]



ggplot() + geom_line(data=df_movil, aes(x=DATETIME,y=CO_S_MA6)) + geom_line(data=test,aes(x=DATETIME,y=prediccion),color='blue')


# 1 REGISTRO POR DIA



df_g <- df2 %>%  group_by(DATE) %>%  summarise (across(everything(), mean))

df_g$DATE <- as.Date(as.character(df_g$DATE),format="%Y-%m-%d")

df_g <- df_g  %>% as_tsibble(index= DATE)
df_g <- df_g %>% mutate("CO_S_MA6" = movavg(df_g$CO_S_IMP, 6,"s"))


#CO_S_IMP

df_g$ds <- df_g$DATE
df_g$y <- df_g$CO_S_IMP


train <- df_g[df_g$Date < "2005-02-15",]
test <- df_g[df_g$Date >= "2005-02-15",]



m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=25)
m<-fit.prophet(m,train)


future <- make_future_dataframe(m, periods = 49)

forecast <- predict(m,future)

plot(m, forecast)


prediccion = append(train[342,]$CO_S_IMP,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]

length(df_g$CO_S_IMP)
length(test$prediccion)

ggplot() + geom_line(data=df_g, aes(x=DATE,y=CO_S_IMP)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')


#CO_S_MA6

df_g$ds <- df_g$DATE
df_g$y <- df_g$CO_S_MA6


train <- df_g[df_g$Date < "2005-02-15",]
test <- df_g[df_g$Date >= "2005-02-15",]



m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=15)
m<-fit.prophet(m,train)


future <- make_future_dataframe(m, periods = 49)

forecast <- predict(m,future)

plot(m, forecast)


prediccion = append(train[342,]$CO_S_MA6,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]

length(df_g$CO_S_MA6)
length(test$prediccion)

ggplot() + geom_line(data=df_g, aes(x=DATE,y=CO_S_MA6)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')
