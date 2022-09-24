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
library (fpp)


setwd("~/Documents/Data-Mining/Series_Temporales/TP-Series-Temporales/")


############################
#AGRUPO 3 REGISTROs POR DIA#
############################


#Levanto Dataset
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

#Veo %nulos columnas
sapply(df, function(x){sum(is.na(x)/dim(df)[1])})

gases_filtro <- c("CO_S","NMHC_S","NOX_S","NO2_S","O3_S","T","RH","AH" )


#Hago imputacion de media movil para los nulos
df2 <- df
for(i in gases_filtro){
  df2[paste0(i,'_IMP')]<- na_ma(df2[i], k = 7, weighting = "exponential")
}


#AS TSIBBLE
gases_imp <- c("CO_S_IMP","NMHC_S_IMP","NOX_S_IMP","NO2_S_IMP","O3_S_IMP","T_IMP","RH_IMP","AH_IMP" )

df3 <- df2[c("DATE",gases_imp)]

#Genero una columna que me agrupa si es mañana, tarde o noche.

df3 = df3 %>% mutate(df,
               TimeGroup = case_when(
                 TIME < "08:00:00" ~ "Morning",
                 TIME < "16:00:00" ~ "Evening",
                 TIME <= "24:00:00" ~ "Night",
                 TRUE ~ NA_character_)
)

#df_g <- df3 %>%  group_by(DATE,TimeGroup) %>%  summarise (across(everything(), mean))

df_g <- df3 %>%  group_by(DATE) %>%  summarise (across(everything(), mean))

#df_g <- df_g  %>% as_tsibble(index= DATETIME)
df_g$DATE <- as.Date(as.character(df_g$DATE),format="%Y-%m-%d")

df_g <- df_g  %>% as_tsibble(index= DATE)



##########
#AUTOPLOT#
##########


jpeg("CO_S_IMP.jpg")
df_g %>% autoplot(CO_S_IMP, colour = 'black') +   labs( x= "DateTime") 
dev.off()

jpeg("NMHC_S_IMP")
df_g %>% autoplot(NMHC_S_IMP, colour = 'black') +   labs( x= "DateTime") 
dev.off()

jpeg("NOX_S_IMP")
df_g %>% autoplot(NOX_S_IMP, colour = 'black') +   labs( x= "DateTime") 
dev.off()

jpeg("NO2_S_IMP")
df_g %>% autoplot(NO2_S_IMP, colour = 'black') +   labs(x= "DateTime") 
dev.off()

jpeg("O3_S_IMP")
df_g %>% autoplot(O3_S_IMP, colour = 'black') +   labs( x= "DateTime") 
dev.off()

jpeg("T_IMP")
df_g %>% autoplot(T_IMP, colour = 'black') +   labs(x= "DateTime") 
dev.off()

jpeg("RH_IMP")
df_g %>% autoplot(RH_IMP, colour = 'black') +   labs( x= "DateTime") 
dev.off()

jpeg("AH_IMP")
df_g %>% autoplot(AH_IMP, colour = 'black') +   labs( x= "DateTime") 
dev.off()


#################
#TEST STATIONARY#
#################

#CO_S_IMP 
#ESTACIONARIA
kpss.test(df_g$CO_S_IMP) #p-value = 0.1 
adf.test(df_g$CO_S_IMP) #p-value = 0.01


#NMHC_S_IMP
#ESTACIONARIA
kpss.test(df_g$NMHC_S_IMP) #p-value = 0.07207
adf.test(df_g$NMHC_S_IMP) #p-value = 0.01

#NOX_S_IMP
#INDEFINIDA
kpss.test(df_g$NOX_S_IMP) #p-value = 0.01
adf.test(df_g$NOX_S_IMP) #p-value = 0.01

#NO2_S_IMP
#INDEFINIDA
kpss.test(df_g$NO2_S_IMP) #p-value = 0.01
adf.test(df_g$NO2_S_IMP) #p-value = 0.01

#O3_S_IMP
#INDEFINIDA
kpss.test(df_g$O3_S_IMP) #p-value = 0.01
adf.test(df_g$O3_S_IMP) #p-value = 0.01

#T_IMP
#ESTACIONARIA
kpss.test(df_g$T_IMP) #p-value = 0.01
adf.test(df_g$T_IMP) #p-value = 0.2704 3 valores, 1 valor = 0.6214

#RH_IMP
#INDEFINIDA
kpss.test(df_g$RH_IMP) #p-value = 0.01
adf.test(df_g$RH_IMP) #p-value = 0.01 un valor = 0.0279

#AH_IMP
#INDEFINIDA
kpss.test(df_g$AH_IMP) #p-value = 0.01
adf.test(df_g$AH_IMP) #p-value = 0.01 1 vañpr = 0.2872


######################
#TEST STATIONARY DIFF#
######################

gases_imp <- c("CO_S_IMP","NMHC_S_IMP","NOX_S_IMP","NO2_S_IMP","O3_S_IMP","T_IMP","RH_IMP","AH_IMP" )
#HAGO UN DIFFERENCE

df_g$CO_S_DIFF = c(0,diff(df_g$CO_S_IMP)) 
df_g$NMHC_S_DIFF = c(0,diff(df_g$NMHC_S_IMP)) 
df_g$NOX_S_DIFF = c(0,diff(df_g$NOX_S_IMP)) 
df_g$NO2_S_DIFF = c(0,diff(df_g$NO2_S_IMP)) 
df_g$O3_S_DIFF = c(0,diff(df_g$O3_S_IMP)) 
df_g$T_DIFF = c(0,diff(df_g$T_IMP)) 
df_g$RH_DIFF = c(0,diff(df_g$RH_IMP)) 
df_g$AH_DIFF = c(0,diff(df_g$AH_IMP)) 


df_diff <- df_g[2:dim(df_g)[1],]
#CO_S_IMP
#ESTACIONARIA
kpss.test(df_diff$CO_S_DIFF) #p-value = 0.1
adf.test(df_diff$CO_S_DIFF) #p-value = 0.01

#NMHC_S_IMP
#ESTACIONARIA
kpss.test(df_diff$NMHC_S_DIFF) #p-value = 0.1
adf.test(df_diff$NMHC_S_DIFF) #p-value = 0.01

#NOX_S_IMP
#ESTACIONARIA
kpss.test(df_diff$NOX_S_DIFF) #p-value = 0.1
adf.test(df_diff$NOX_S_DIFF) #p-value = 0.01

#NO2_S_IMP
#ESTACIONARIA
kpss.test(df_diff$NO2_S_DIFF) #p-value = 0.1
adf.test(df_diff$NO2_S_DIFF) #p-value = 0.01

#O3_S_IMP
#ESTACIONARIA
kpss.test(df_diff$O3_S_DIFF) #p-value = 0.1
adf.test(df_diff$O3_S_DIFF) #p-value = 0.01

#T_IMP
#ESTACIONARIA
kpss.test(df_diff$T_DIFF) #p-value = 0.1
adf.test(df_diff$T_DIFF) #p-value = 0.01

#RH_IMP
#ESTACIONARIA
kpss.test(df_diff$RH_DIFF) #p-value = 0.1
adf.test(df_diff$RH_DIFF) #p-value = 0.01

#AH_IMP
#ESTACIONARIA
kpss.test(df_diff$AH_DIFF) #p-value = 0.1
adf.test(df_diff$AH_DIFF) #p-value = 0.01

###############
#AUTOPLOT DIFF#
###############
#ACA SOLO PLOTEO TEMPERATURA Y RH

jpeg("RH_DIFF")
df_diff %>% autoplot(RH_DIFF, colour = 'black') +   labs( x= "DateTime") 
dev.off()

jpeg("T_DIFF")
df_diff %>% autoplot(T_DIFF, colour = 'black') +   labs( x= "DateTime") 
dev.off()


#############
#FORECASTING#
#############

#DESCOMPOSICION
#RH
dcmp <- df_diff %>%
  model(stl = STL(RH_DIFF))

jpeg("COMPONENTS_RH_DIFF")
components(dcmp) %>% autoplot()
dev.off()

#T
dcmp <- df_diff %>%
  model(stl = STL(T_IMP))

jpeg("COMPONENTS_T_IMP")
components(dcmp) %>% autoplot()
dev.off()

#S-ARIMA 
#T

train <- df_g[df_g$DATE< "2005-02-15",]
test  <-  df_g[df_g$DATE >= "2005-02-15",]

train <- df_g[df_diff$DATE< "2005-02-15",]
test  <-  df_g[df_diff$DATE >= "2005-02-15",]

Lambda<- BoxCox.lambda(train$T_DIFF)

myts <- ts(train$T_IMP)

#myts <- ts(df_movil[,c("DateTime","MA12")]$MA12, frequency=24L)
fit_am = auto.arima(myts, seasonal = T,parallel = TRUE, ic ="aic",stepwise=FALSE, trace=TRUE,lambda='auto')

jpeg("ARIMA T_IMP")
plot ( forecast ( fit_am,h=49),include=80,ylab="RH_DIFF")
dev.off()


plot ( forecast ( fit_am,h=10),include=80)

forecast_T <- forecast(fit_am,h=49,level=c(80))

predict_T<-predict(fit_am, n.ahead=49)

pred_con_ult_train = append(train[352,]$T_IMP,predict_T$pred)

test$prediccion <- pred_con_ult_train[-1]

ggplot() + geom_line(data=df_g, aes(x=DATE,y=T_IMP)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')


par(mfrow=c(1,2))
Acf(myts, main="")
Pacf(myts, main="")
fit .arima2 <− Arima(usconsumption[,1], order=c(0,0,3))
print ( fit .arima2)


#T DIFF



train <- df_g[df_g$DATE< "2005-02-15",]
test  <-  df_g[df_g$DATE >= "2005-02-15",]

train <- df_g[df_diff$DATE< "2005-02-15",]
test  <-  df_g[df_diff$DATE >= "2005-02-15",]

Lambda<- BoxCox.lambda(train$T_DIFF)

myts <- ts(train$T_DIFF)

#myts <- ts(df_movil[,c("DateTime","MA12")]$MA12, frequency=24L)
fit_am = auto.arima(myts, seasonal = T,parallel = TRUE, ic ="aic",stepwise=FALSE, trace=TRUE,lambda='auto')


#plot ( forecast ( fit_am,h=10),include=80)

forecast_T <- forecast(fit_am,h=49,level=c(80))

predict_T<-predict(fit_am, n.ahead=49)

pred_con_ult_train = append(train[352,]$T_DIFF,predict_T$pred)

test$prediccion <- pred_con_ult_train[-1]

ggplot() + geom_line(data=df_g, aes(x=DATE,y=T_DIFF)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')


par(mfrow=c(1,2))
Acf(myts, main="")
Pacf(myts, main="")



#RH DIFF



#RH DIFF

train <- df_g[df_diff$DATE< "2005-02-15",]
test  <-  df_g[df_diff$DATE >= "2005-02-15",]


myts <- ts(train$RH_DIFF)

#myts <- ts(df_movil[,c("DateTime","MA12")]$MA12, frequency=24L)
fit_am = auto.arima(myts, seasonal = T,parallel = TRUE, ic ="aic",stepwise=FALSE, trace=TRUE,lambda='auto')

jpeg("ARIMA RH_DIFF")
plot ( forecast ( fit_am,h=49),include=80,ylab="RH_DIFF")
dev.off()

forecast_T <- forecast(fit_am,h=49,level=c(80))

predict_T<-predict(fit_am, n.ahead=49)

pred_con_ult_train = append(train[352,]$RH_DIFF,predict_T$pred)

test$prediccion <- pred_con_ult_train[-1]

ggplot() + geom_line(data=df_g, aes(x=DATE,y=RH_DIFF)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')


par(mfrow=c(1,2))
Acf(myts, main="")
Pacf(myts, main="")


#########
#PROPHET#
#########

#T_IMP

df_g <- df_g %>% mutate("T_MA3" = movavg(df_g$T_IMP, 3,"s"))
df_g <- df_g %>% mutate("RH_MA3" = movavg(df_g$RH_IMP, 3,"s"))

#PARA T_IMP NORMAL
df_g$ds <- df_g$DATE
df_g$y <- df_g$T_IMP
train <- df_g[df_g$DATE < "2005-02-15",]
test <- df_g[df_g$DATE >= "2005-02-15",]

m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=20)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$T_IMP,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]
ggplot() + geom_line(data=df_g, aes(x=DATE,y=T_IMP)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')


#PARA T_IMP Suavizado
df_g$ds <- df_g$DATE
df_g$y <- df_g$T_MA3
train <- df_g[df_g$DATE < "2005-02-15",]
test <- df_g[df_g$DATE >= "2005-02-15",]
train <- train  %>% as_tsibble(index= ds)

m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=20)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$T_IMP,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]

jpeg("PROPHET_T_IMP")
ggplot() + geom_line(data=df_g, aes(x=DATE,y=T_IMP)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')
dev.off()

jpeg("PROPHET_T_IMP_PURO")
plot(m,forecast)
dev.off()

#RH_DIFF

df_g <- df_g %>% mutate("T_MA3" = movavg(df_g$T_IMP, 3,"s"))
df_g <- df_g %>% mutate("RH_MA3" = movavg(df_g$RH_IMP, 3,"s"))

df_diff <- df_diff %>% mutate("RH_MA3" = movavg(df_diff$RH_DIFF, 3,"s"))
df_diff <- df_diff %>% mutate("AH_MA3" = movavg(df_diff$AH_DIFF, 3,"s"))


#PARA RH_DIFF NORMAL
df_diff$ds <- df_diff$DATE
df_diff$y <- df_diff$RH_DIFF
train <- df_diff[df_diff$DATE < "2005-02-15",]
test <- df_diff[df_diff$DATE >= "2005-02-15",]
m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=60)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$RH_DIFF,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]
ggplot() + geom_line(data=df_diff, aes(x=DATE,y=RH_DIFF)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')


#PARA RH_DIFF SUAVIZADO
df_diff$ds <- df_diff$DATE
df_diff$y <- df_diff$RH_MA3
train <- df_diff[df_diff$DATE < "2005-02-15",]
test <- df_diff[df_diff$DATE >= "2005-02-15",]
m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=55)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$RH_MA3,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]

jpeg("PROPHET_RH_DIFF")
ggplot() + geom_line(data=df_diff, aes(x=DATE,y=RH_DIFF)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')
dev.off()





#PARA AH_DIFF SUAVIZADO
df_diff$ds <- df_diff$DATE
df_diff$y <- df_diff$AH_MA3
train <- df_diff[df_diff$DATE < "2005-02-15",]
test <- df_diff[df_diff$DATE >= "2005-02-15",]
m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=50)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
jpeg("PROPHET_AH_PURO")
plot(m,forecast)
dev.off()

train <- train  %>% as_tsibble(index= ds)


prediccion = append(train[342,]$AH_MA3,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]

jpeg("PROPHET_AH_DIFF")
ggplot() + geom_line(data=df_diff, aes(x=DATE,y=AH_DIFF)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')
dev.off()