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

setwd("~/Documents/Data-Mining/Series_Temporales/TP-Series-Temporales/")





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




#AS TSIBBLE
gases_imp <- c("CO_S_IMP","NMHC_S_IMP","NOX_S_IMP","NO2_S_IMP","O3_S_IMP","T_IMP","RH_IMP","AH_IMP" )

df3 <- df2[c("DATE",gases_imp)]

#######################
# 1 REGISTRO POR DIA  #
#######################


df_g <- df3 %>%  group_by(DATE) %>%  summarise (across(everything(), mean))

df_g$DATE <- as.Date(as.character(df_g$DATE),format="%Y-%m-%d")

df_g <- df_g  %>% as_tsibble(index= DATE)


###########################
#FILTRO POR MOVING AVERAGE#
###########################

df_g <- df_g %>% mutate("CO_S_MA3" = movavg(df_g$CO_S_IMP, 3,"s"))
df_g <- df_g %>% mutate("NMHC_S_MA3" = movavg(df_g$NMHC_S_IMP, 3,"s"))
df_g <- df_g %>% mutate("NOX_S_MA3" = movavg(df_g$NOX_S_IMP, 3,"s"))
df_g <- df_g %>% mutate("NO2_S_MA3" = movavg(df_g$NO2_S_IMP, 3,"s"))
df_g <- df_g %>% mutate("O3_S_MA3" = movavg(df_g$O3_S_IMP, 3,"s"))
df_g <- df_g %>% mutate("T_MA3" = movavg(df_g$T_IMP, 3,"s"))
df_g <- df_g %>% mutate("RH_MA3" = movavg(df_g$RH_IMP, 3,"s"))
df_g <- df_g %>% mutate("AH_MA3" = movavg(df_g$AH_IMP, 3,"s"))

gases_finales <- colnames(df_g)[10:17]

#CO_S_MA3
df_g$ds <- df_g$DATE
df_g$y <- df_g$CO_S_MA3
train <- df_g[df_g$DATE < "2005-02-15",]
test <- df_g[df_g$DATE >= "2005-02-15",]

m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=17)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$CO_S_MA3,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]
ggplot() + geom_line(data=df_g, aes(x=DATE,y=CO_S_MA3)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')


#NMHC_S_MA6
df_g$ds <- df_g$DATE
df_g$y <- df_g$NMHC_S_MA3
train <- df_g[df_g$DATE < "2005-02-15",]
test <- df_g[df_g$DATE >= "2005-02-15",]
m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=15)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$NMHC_S_MA3,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]
ggplot() + geom_line(data=df_g, aes(x=DATE,y=NMHC_S_MA3)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')


#NOX_S_MA6
df_g$ds <- df_g$DATE
df_g$y <- df_g$NOX_S_MA3
train <- df_g[df_g$DATE < "2005-02-15",]
test <- df_g[df_g$DATE >= "2005-02-15",]
m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=15)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$NOX_S_MA3,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]
ggplot() + geom_line(data=df_g, aes(x=DATE,y=NOX_S_MA3)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')


#NO2_S_MA6
df_g$ds <- df_g$DATE
df_g$y <- df_g$NO2_S_MA3
train <- df_g[df_g$DATE < "2005-02-15",]
test <- df_g[df_g$DATE >= "2005-02-15",]
m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=15)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$NO2_S_MA3,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]
ggplot() + geom_line(data=df_g, aes(x=DATE,y=NO2_S_MA3)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')



#O3_S_MA6
df_g$ds <- df_g$DATE
df_g$y <- df_g$O3_S_MA3
train <- df_g[df_g$DATE < "2005-02-15",]
test <- df_g[df_g$DATE >= "2005-02-15",]
m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=15)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$O3_S_MA3,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]
ggplot() + geom_line(data=df_g, aes(x=DATE,y=O3_S_MA3)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')



#T_MA6
df_g$ds <- df_g$DATE
df_g$y <- df_g$T_MA3
train <- df_g[df_g$DATE < "2005-02-15",]
test <- df_g[df_g$DATE >= "2005-02-15",]
m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=15)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$T_MA3,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]
ggplot() + geom_line(data=df_g, aes(x=DATE,y=T_MA3)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')



#RH_MA6
df_g$ds <- df_g$DATE
df_g$y <- df_g$RH_MA3
train <- df_g[df_g$DATE < "2005-02-15",]
test <- df_g[df_g$DATE >= "2005-02-15",]
m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=10)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$RH_MA3,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]
ggplot() + geom_line(data=df_g, aes(x=DATE,y=RH_MA3)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')



#AH_MA6
df_g$ds <- df_g$DATE
df_g$y <- df_g$AH_MA3
train <- df_g[df_g$DATE < "2005-02-15",]
test <- df_g[df_g$DATE >= "2005-02-15",]
m <- prophet()
m <- add_seasonality(m, name ='yearly',period=365,fourier.order=17)
m<-fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 49)
forecast <- predict(m,future)
prediccion = append(train[342,]$AH_MA3,forecast$yhat[343:391])
test$prediccion <- prediccion[-1]
ggplot() + geom_line(data=df_g, aes(x=DATE,y=AH_MA3)) + geom_line(data=test,aes(x=DATE,y=prediccion),color='blue')

