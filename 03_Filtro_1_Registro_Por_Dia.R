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

gases_filtro_imp <- paste0(gases_filtro,'_imp')


#Nuevo Dataset tsibble con date como indice
df_g$Date <- as.Date(as.character(df_g$Date),format="%Y-%m-%d")
df <- df_g[,c("Date",gases_filtro_imp)]  %>% as_tsibble(index= Date)



#############
#TEMPERATURA#
#############




#Plot temperatura
df %>% autoplot(T_imp, colour = 'black') +   labs(y  = "Temperatura", x= "DateTime") 


#Componentes temperatura
dcmp <- df %>%
  model(stl = STL(T_imp))

components(dcmp) %>% autoplot()

kpss.test(df$T_imp)
adf.test(df$T_imp,k = 0)



#Train Test


train <- df[df$Date < "2005-03-01",]
test <- df[df$Date >= "2005-03-01",]
myts <- ts(train$T_imp)
fit_am = auto.arima(myts, seasonal = T,parallel = TRUE, ic ="aic",stepwise=FALSE, trace=TRUE,lambda='auto')


forecast_3<-forecast(fit_am)
predict_3<-predict(fit_am, n.ahead=35)
pred_con_ult_train_3 = append(train[356,]$T_imp,predict_3$pred)
test$prediccion <- pred_con_ult_train_3[-1]
ggplot() + geom_line(data=df, aes(x=Date,y=T_imp)) + geom_line(data=test,aes(x=Date,y=prediccion),color='blue')


#BOXCOX Y DIFF
lambda <- BoxCox.lambda(df$T_imp)

df_diff <- df %>% mutate(boxcox_T= (BoxCox(T_imp,lambda))) 
df_diff <- df_diff %>% mutate(diff_T= difference(BoxCox(T_imp,lambda),1)) 


df_diff %>%   gg_tsdisplay(diff_T, plot_type='partial')

train_ts_ds_diff = df_diff %>% slice(-(331:391))
test_ts_ds_diff = df_diff %>% slice(331:391)

myts_diff <- ts(train_ts_ds_diff$diff_T)

fit_am_diff = auto.arima(myts_diff, seasonal =F, ic ="aic", trace=TRUE)

forecast_diff<-predict(fit_am_diff, n.ahead=60)
pred_con_ult_train_diff = append(train_ts_ds_diff[330,]$diff_T,forecast_diff$pred)


#Grafico Transformado
test_ts_ds_diff$prediccion <- pred_con_ult_train_diff[-1]
#Grafico invirtiendo todo
pred_inv_diff <-c()
pred_inv_diff[1] <- train_ts_ds_diff$boxcox_T[330]
for(i in 2:length(pred_con_ult_train_diff)){
  pred_inv_diff[i] <- pred_inv_diff[i-1]+pred_con_ult_train_diff[i]
}
pred_transformed <- InvBoxCox(pred_inv_diff,lambda)




test_ts_ds_diff$prediccion_transformada = pred_transformed[2:62]


df_diff %>% autoplot(boxcox_T)
ggplot() + geom_line(data=df_diff, aes(x=Date,y=T_imp)) + geom_line(data=test_ts_ds_diff,aes(x=Date,y=prediccion_transformada),color='blue')  