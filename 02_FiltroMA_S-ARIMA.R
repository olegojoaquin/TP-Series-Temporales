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


setwd("~/Documents/Data-Mining/Series_Temporales/TP-Series-Temporales//")

df <- read.csv("AirQualityUCI.csv")



df$DateTime <-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")



df <- df[which(is.na(df$DateTime) ==FALSE),]


df <- df  %>% as_tsibble(index= DateTime)


gases <- c("CO.GT.","PT08.S1.CO.","NMHC.GT.","C6H6.GT.","PT08.S2.NMHC.","NOx.GT.","PT08.S3.NOx.","NO2.GT.",
           "PT08.S4.NO2.","PT08.S5.O3.","T","RH","AH" )

gases_filtro <- c("PT08.S1.CO.","C6H6.GT.","PT08.S2.NMHC.","PT08.S3.NOx.","PT08.S4.NO2.","PT08.S5.O3.","T","RH","AH" )


df[df == -200] <- NA

sapply(df, function(x){sum(is.na(x)/dim(df)[1])})



#######################
#ANALIZO TEMPERATURA
######################


df$T_imp <- na_ma(df$T, k = 7, weighting = "exponential")


dcmp <- df %>%
  model(stl = STL(T_imp))

components(dcmp) %>% autoplot()

df_movil <- df %>% mutate("MA12" = movavg(df$T_imp, 12,"s"))

df_movil <- df_movil %>% mutate("MA6" = movavg(df_movil$T_imp, 6,"s"))

df_movil <- df_movil %>% mutate("MA3" = movavg(df_movil$T_imp, 3,"s"))

df_movil %>% autoplot(T_imp, colour = 'black') +   labs(y  = "Temperatura", x= "DateTime") 
df_movil %>% autoplot(MA6, colour = 'red') +   labs(y  = "Temperatura", x= "DateTime") 


df_movil %>% autoplot(T_612, colour = 'red') +   labs(y  = "Temperatura", x= "DateTime") 


#BOXCOX
min(df_movil$DateTime)
max(df_movil$DateTime)

train <- df_movil[df_movil$DateTime< "2005-03-01 00:00:00 -03",]
test  <-  df_movil[df_movil$DateTime >= "2005-03-01 00:00:00 -03",]


Lambda<- BoxCox.lambda(train)

myts <- ts(train$MA6)

#myts <- ts(df_movil[,c("DateTime","MA12")]$MA12, frequency=24L)
fit_am = auto.arima(myts, seasonal = F,parallel = TRUE, ic ="aic",stepwise=FALSE, trace=TRUE,lambda='auto')

forecast_T <- forecast(fit_am,h=1503,level=c(95))

predict_T<-predict(fit_am, n.ahead=1503)

pred_con_ult_train = append(train[7854,]$MA6,predict_T$pred)

test$prediccion <- pred_con_ult_train[-1]

ggplot() + geom_line(data=df_movil, aes(x=DateTime,y=MA6)) + geom_line(data=test,aes(x=DateTime,y=prediccion),color='blue')


forecast_complex1<-forecast(fit_am,xreg=test)

accuracy(f = forecast_complex1,x = test)

checkresiduals(fit_am)

library(tseries)

kpss.test(df_movil$T_imp)
adf.test(df_movil$T_imp,k = 0)

#VEO DIFERENCIA
df_diff <- df_movil %>% mutate(diff_T = difference(MA3)) 

df_diff %>% autoplot(diff_T, colour = 'red') +   labs(y  = "Temperatura", x= "DateTime") 

df_diff <- df_diff[which(is.na(df_diff$diff_T)==FALSE),]

kpss.test(df_diff$diff_T)
adf.test(df_diff$diff_T,k = 0)


