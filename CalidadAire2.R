#Attribute Information:

#0 Date (DD/MM/YYYY)
#1 Time (HH.MM.SS)
#2 True hourly averaged concentration CO in mg/m^3 (reference analyzer)
#3 PT08.S1 (tin oxide) hourly averaged sensor response (nominally CO targeted)
#4 True hourly averaged overall Non Metanic HydroCarbons concentration in microg/m^3 (reference analyzer)
#5 True hourly averaged Benzene concentration in microg/m^3 (reference analyzer)
#6 PT08.S2 (titania) hourly averaged sensor response (nominally NMHC targeted)
#7 True hourly averaged NOx concentration in ppb (reference analyzer)
#8 PT08.S3 (tungsten oxide) hourly averaged sensor response (nominally NOx targeted)
#9 True hourly averaged NO2 concentration in microg/m^3 (reference analyzer)
#10 PT08.S4 (tungsten oxide) hourly averaged sensor response (nominally NO2 targeted)
#11 PT08.S5 (indium oxide) hourly averaged sensor response (nominally O3 targeted)
#12 Temperature in Â°C
#13 Relative Humidity (%)
#14 AH Absolute Humidity

setwd ("D:/SERIES-TEMPORALES")
install.packages("forecast")
install.packages("tsibble")
install.packages("fable")
install.packages("broom")
install.packages ("feasts")
install.packages("ggplot2")
install.packages("zoo")
install.packages("imputeTS")
install.packages("openair")
install.packages("tseries")
install.packages("slider")
install.packages("Rssa")
install.packages("TSA", dep=TRUE)
install.packages("psdr")
install.packages ("tsbox")
install.packages("patchwork")
                   
            
library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(forecast)
library(tsibble)
library(fable)
library(broom)
library(feasts)
library(ggplot2)
library(zoo)
library(imputeTS)
library(dplyr)
library(openair)
library(tseries)
library(slider)
library(Rssa)
library(TSA)
library(psdr)
library(tsbox)
library(patchwork)



df <- read.csv("AirQualityUCI.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

#Unir variable fecha y hora que estan separadas
df$DateTime<-as.POSIXct(paste(df$Date,df$Time),
                        format = "%d/%m/%Y %H:%M:%S")
df$DateTime = as.POSIXct(df$DateTime)

any(is.na(df$DateTime)) 
#Borrar los registros que tienen la variable 
#temporal nula
df<-df[which(is.na(df$DateTime)==FALSE),]

#Para calcular valores de tendencia central y
#dispersion pasar coma a punto en variables 
#que estan como character 

df$CO.GT. <- as.numeric(sub(",", ".", df$CO.GT.))
df$C6H6.GT. <- as.numeric(sub(",", ".", df$C6H6.GT.))
df$T <- as.numeric(sub(",", ".", df$T))
df$RH <- as.numeric(sub(",", ".", df$RH))
df$AH <- as.numeric(sub(",", ".", df$AH))

#Reemplazar los valores -200 por NA 
df[df==-200] <- NA
# determinación del porcentaje de valores perdidos respecto del total de datos
porcentajeMiss <- function(x) {sum(is.na(x)) /length(x) *100}
apply(df, 2, porcentajeMiss)

#ordenar 
df = df[, c(1:5, 7, 6, 8:16)]


#Modificar los nombres de las variables 
dataset_columns = c('Date', 'Time','CO', 'CO_S', 'NMHC', 'NMHC_S', 'C6H6',
                    'NOX', 'NOX_S', 'NO2', 'NO2_S', 'O3_S','T', 'RH', 'AH',"DateTime")


colnames(df) = dataset_columns

#Guardar la informacion como tsibble con la funcion
# tsibble () objeto ST en R 
df_ts <- df %>%  as_tsibble(index= DateTime)

#Las funciones year(), month(), day(), hour(), 
#minute(), y second() nos permiten extraer ese 
#componente de la fecha.

#Para frecuencia diaria= as.date

#Como tsibble
df_diario_ts <- df_ts %>%
  mutate(DIA= as.Date(DateTime))%>%
  index_by(DIA) %>%
  summarise_at(c('CO', 'CO_S', 'NMHC', 'NMHC_S', 'C6H6',
                 'NOX', 'NOX_S', 'NO2', 'NO2_S', 'O3_S','T', 'RH', 'AH'), mean, na.rm=TRUE)

#Como data.frame 
df_diario <- df %>%
  mutate(DIA= as.Date(DateTime))%>%
  group_by(DIA) %>%
  summarise_at(c('CO', 'CO_S', 'NMHC', 'NMHC_S', 'C6H6',
                 'NOX', 'NOX_S', 'NO2', 'NO2_S', 'O3_S','T', 'RH', 'AH'), mean, na.rm=TRUE)

#Mostraria graficos con las medias y boxplot 

summary(df_diario [2:14])
boxplot(df_diario[,2:14], ylab= "Valores")


#Graficos Gases (todos para mostrar caract grales)
#Promedio diario 

P1 <- autoplot(df_diario_ts,CO) +
  labs(title = "CO",
       y = "CO")

P2 <- autoplot(df_diario_ts,CO_S) +
  labs(title = "CO_S",
       y = "CO_S")
P3 <-autoplot(df_diario_ts,NMHC) +
  labs(title = "NMHC",
       y = "NMHC")
P4 <-autoplot(df_diario_ts,NMHC_S) +
  labs(title = "NMHC_S",
       y = "NMHC_S")
P5 <-autoplot(df_diario_ts,C6H6) +
  labs(title = "C6H6",
       y = "C6H6")
P6 <-autoplot(df_diario_ts,NOX) +
  labs(title = "NOX",
       y = "NOX")
P7 <-autoplot(df_diario_ts,NOX_S) +
  labs(title = "NOX_S",
       y = "NOX_S")
P8 <-autoplot(df_diario_ts,NO2) +
  labs(title = "NO2",
       y = "NO2")
P9 <-autoplot(df_diario_ts,NO2_S) +
  labs(title = "NO2_S",
       y = "NO2_S")

P10 <-autoplot(df_diario_ts,O3_S) +
  labs(title = "O3_S",
       y = "O3_S")

P11 <-autoplot(df_diario_ts,T) +
  labs(title = "T",
       y = "T")

P12 <-autoplot(df_diario_ts,RH) +
  labs(title = "RH",
       y = "RH")

P13 <-autoplot(df_diario_ts,AH) +
  labs(title = "AH",
       y = "AH")

# Probar grafico integrado 

P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9 + P10 + P11 + P12 + P13

# las variables que quedan tienen valores NA (missing)
#Hay que estimar (imputar) esos datos (reemplazo
#con valores predichos desde los datos presentes)

#Imputacion por MA de todas las series K=5

Uso <- c('CO', 'CO_S', 'NMHC', 'NMHC_S', 'C6H6',
'NOX', 'NOX_S', 'NO2', 'NO2_S', 'O3_S','T', 'RH', 'AH')


df_imp <- df_diario_ts
  for(i in Uso){
  df_imp[paste0(i,'_IMP')]<- na_ma(df_imp[i], k = 5, weighting = "exponential")
  }

#Graficos imputacion 
ggplot_na_imputations(df_diario_ts$CO, df_imp$CO_IMP) + 
  theme_classic()

ggplot_na_imputations(df_diario_ts$CO_S, df_imp$CO_S_IMP) + 
  theme_classic()

ggplot_na_imputations(df_diario_ts$NMHC_S, df_imp$NMHC_S_IMP) + 
  theme_classic()

ggplot_na_imputations(df_diario_ts$NOX_S, df_imp$NOX_S_IMP) + 
  theme_classic()

ggplot_na_imputations(df_diario_ts$NO2_S, df_imp$NO2_S_IMP) + 
  theme_classic()

ggplot_na_imputations(df_diario_ts$O3_S, df_imp$O3_S_IMP) + 
  theme_classic()


#Filtro con gases promedio del dia 
#Prueba con series- k=3 

#imputamos mas sumamos un filtro de ma 

#CO_S

df_prueba <- df_imp %>%
  mutate("3-MA" = slider::slide_dbl(CO_S_IMP, mean,
                                    .before = 2, .after = 0, .complete = TRUE))

df_prueba %>%
  autoplot(CO_S_IMP) +
  geom_line(aes(y = `3-MA`), colour = "red") +
  labs(y = "Valor CO",
       title = "CO") +
  guides(colour = guide_legend(title = "series"))

# NMHC_S

  df_prueba <- df_imp %>%
    mutate("3-MA" = slider::slide_dbl(NMHC_S_IMP, mean,
                                      .before = 2, .after = 0, .complete = TRUE))
  
  df_prueba %>%
    autoplot(NMHC_S_IMP) +
    geom_line(aes(y = `3-MA`), colour = "red") +
    labs(y = "Valor NMHC_S",
         title = "NMHC_S") +
    guides(colour = guide_legend(title = "series"))

#NOX_S

  df_prueba <- df_imp %>%
    mutate("3-MA" = slider::slide_dbl(NOX_S_IMP, mean,
                                      .before = 2, .after = 0, .complete = TRUE))
  
  df_prueba %>%
    autoplot(NOX_S_IMP) +
    geom_line(aes(y = `3-MA`), colour = "red") +
    labs(y = "Valor NOX_S",
         title = "NOX_S") +
    guides(colour = guide_legend(title = "series"))

#NO2_S

  df_prueba <- df_imp %>%
    mutate("3-MA" = slider::slide_dbl(NO2_S_IMP, mean,
                                      .before = 2, .after = 0, .complete = TRUE))
  
  df_prueba %>%
    autoplot(NO2_S_IMP) +
    geom_line(aes(y = `3-MA`), colour = "red") +
    labs(y = "Valor NO2_S",
         title = "NO2_S") +
    guides(colour = guide_legend(title = "series"))

#O3_S

  df_prueba <- df_imp %>%
    mutate("3-MA" = slider::slide_dbl(O3_S_IMP, mean,
                                      .before = 2, .after = 0, .complete = TRUE))
  
  df_prueba %>%
    autoplot(O3_S_IMP) +
    geom_line(aes(y = `3-MA`), colour = "red") +
    labs(y = "Valor O3_S",
         title = "O3_S") +
    guides(colour = guide_legend(title = "series"))

#T

  df_prueba <- df_imp %>%
    mutate("3-MA" = slider::slide_dbl(T_IMP, mean,
                                      .before = 2, .after = 0, .complete = TRUE))
  
  df_prueba %>%
    autoplot(T_IMP) +
    geom_line(aes(y = `3-MA`), colour = "red") +
    labs(y = "Valor T",
         title = "T") +
    guides(colour = guide_legend(title = "series"))

  #RH

  df_prueba <- df_imp %>%
    mutate("3-MA" = slider::slide_dbl(RH_IMP, mean,
                                      .before = 2, .after = 0, .complete = TRUE))
  
  df_prueba %>%
    autoplot(RH_IMP) +
    geom_line(aes(y = `3-MA`), colour = "red") +
    labs(y = "Valor RH",
         title = "RH") +
    guides(colour = guide_legend(title = "series"))

#AH
  
  df_prueba <- df_imp %>%
    mutate("3-MA" = slider::slide_dbl(AH_IMP, mean,
                                      .before = 2, .after = 0, .complete = TRUE))
  
  df_prueba %>%
    autoplot(AH_IMP) +
    geom_line(aes(y = `3-MA`), colour = "red") +
    labs(y = "Valor AH",
         title = "AH") +
    guides(colour = guide_legend(title = "series"))  


df_imp <- df_imp %>%
dplyr::mutate(CO_ma3 = zoo::rollmean(CO_IMP, k = 3, fill = NA))

df_imp <- df_imp %>%
  dplyr::mutate(CO_S_ma3 = zoo::rollmean(CO_S_IMP, k = 3, fill = NA))
df_imp <- df_imp %>%
  dplyr::mutate(NMHC_ma3 = zoo::rollmean(NMHC_IMP, k = 3, fill = NA))
df_imp <- df_imp %>%
  dplyr::mutate(NMHC_S_ma3 = zoo::rollmean(NMHC_S_IMP, k = 3, fill = NA))
df_imp <- df_imp %>%
  dplyr::mutate(C6H6_ma3 = zoo::rollmean(C6H6_IMP, k = 3, fill = NA))
df_imp <- df_imp %>%
  dplyr::mutate(NOX_ma3 = zoo::rollmean(NOX_IMP, k = 3, fill = NA))
df_imp <- df_imp %>%
  dplyr::mutate(NOX_S_ma3 = zoo::rollmean(NOX_S_IMP, k = 3, fill = NA))
df_imp <- df_imp %>%
  dplyr::mutate(NO2_ma3 = zoo::rollmean(NO2_IMP, k = 3, fill = NA))
df_imp <- df_imp %>%
  dplyr::mutate(NO2_S_ma3 = zoo::rollmean(NO2_S_IMP, k = 3, fill = NA))
df_imp <- df_imp %>%
  dplyr::mutate(O3_S_ma3 = zoo::rollmean(O3_S_IMP, k = 3, fill = NA))
df_imp <- df_imp %>%
  dplyr::mutate(T_ma3 = zoo::rollmean(T_IMP, k = 3, fill = NA))
df_imp <- df_imp %>%
  dplyr::mutate(RH_ma3 = zoo::rollmean(RH_IMP, k = 3, fill = NA))
df_imp <- df_imp %>%
  dplyr::mutate(AH_ma3 = zoo::rollmean(AH_IMP, k = 3, fill = NA))

#Analisis descriptivo de las TS

#Descomposicion 

#CO_S_ma3

df_imp %>% 
model(classical_decomposition(CO_S_ma3, type = "additive")) %>% 
components() %>% 
autoplot()


#NMHC_S_ma3
df_imp %>% 
  model(classical_decomposition(NMHC_S_ma3, type = "additive")) %>% 
  components() %>% 
  autoplot()

#NOX_S_ma3

df_imp %>% 
  model(classical_decomposition(NOX_S_ma3, type = "additive")) %>% 
  components() %>% 
  autoplot()

#NO2_S_ma3
df_imp %>% 
  model(classical_decomposition(NO2_S_ma3, type = "additive")) %>% 
  components() %>% 
  autoplot()

#O3_S_ma3
df_imp %>% 
  model(classical_decomposition(O3_S_ma3, type = "additive")) %>% 
  components() %>% 
  autoplot()

#T

df_imp %>% 
  model(classical_decomposition(T_ma3, type = "additive")) %>% 
  components() %>% 
  autoplot()

#RH
df_imp %>% 
  model(classical_decomposition(RH_ma3, type = "additive")) %>% 
  components() %>% 
  autoplot()
#AH

df_imp %>% 
  model(classical_decomposition(AH_ma3, type = "additive")) %>% 
  components() %>% 
  autoplot()

#Autocorrelacion

#CO_S
df_imp %>% ACF(CO_S_ma3 )
df_imp %>%
  ACF(CO_S_ma3) %>%
  autoplot() + labs(title="ACF-CO_S_ma3")

#NMHC_S_ma3
df_imp %>% ACF(NMHC_S_ma3 )
df_imp %>%
  ACF(NMHC_S_ma3) %>%
  autoplot() + labs(title="ACF-NMHC_S_ma3")

#NOX_S_ma3
df_imp %>% ACF(NOX_S_ma3 )
df_imp %>%
  ACF(NOX_S_ma3) %>%
  autoplot() + labs(title="ACF-NOX_S_ma3")

#NO2_S_ma3
df_imp %>% ACF(NO2_S_ma3 )
df_imp %>%
  ACF(NO2_S_ma3 ) %>%
  autoplot() + labs(title="ACF-NO2_S_ma3")

#O3_S_ma3
df_imp %>% ACF(O3_S_ma3 )
df_imp %>%
  ACF(O3_S_ma3) %>%
  autoplot() + labs(title="ACF-O3_S_ma3")

#T
df_imp %>% ACF(T_ma3 )
df_imp %>%
  ACF(T_ma3) %>%
  autoplot() + labs(title="ACF-T_ma3")

#RH

df_imp %>% ACF(RH_ma3 )
df_imp %>%
  ACF(RH_ma3) %>%
  autoplot() + labs(title="ACF-RH_ma3")

#AH
df_imp %>% ACF(AH_ma3 )
df_imp %>%
  ACF(AH_ma3) %>%
  autoplot() + labs(title="ACF-AH_ma3")

#correlacion entre dos series 

#Gases y temperatura 

#CO_S

plot(df_imp$CO_S_ma3, df_imp$T_ma3)
cor.test(df_imp$CO_S_ma3, df_imp$T_ma3)
ccf(df_imp$CO_S_ma3, df_imp$T_ma3, na.action = na.pass)
print(ccf(df_imp$CO_S_ma3, df_imp$T_ma3, na.action = na.pass))

#NMHC_S_ma3
plot(df_imp$NMHC_S_ma3, df_imp$T_ma3)
cor.test(df_imp$NMHC_S_ma3, df_imp$T_ma3)
ccf(df_imp$NMHC_S_ma3, df_imp$T_ma3, na.action = na.pass)
print(ccf(df_imp$NMHC_S_ma3, df_imp$T_ma3, na.action = na.pass))

#NOX_S_ma3

plot(df_imp$NOX_S_ma3, df_imp$T_ma3)
cor.test(df_imp$NOX_S_ma3, df_imp$T_ma3)
ccf(df_imp$NOX_S_ma3, df_imp$T_ma3, na.action = na.pass)
print(ccf(df_imp$NOX_S_ma3, df_imp$T_ma3, na.action = na.pass))


#NO2_S_ma3

plot(df_imp$NO2_S_ma3, df_imp$T_ma3)
cor.test(df_imp$NO2_S_ma3, df_imp$T_ma3)
ccf(df_imp$NO2_S_ma3, df_imp$T_ma3, na.action = na.pass)
print(ccf(df_imp$NO2_S_ma3, df_imp$T_ma3, na.action = na.pass))

#O3_S_ma3

plot(df_imp$O3_S_ma3, df_imp$T_ma3)
cor.test(df_imp$O3_S_ma3, df_imp$T_ma3)
ccf(df_imp$O3_S_ma3, df_imp$T_ma3, na.action = na.pass)
print(ccf(df_imp$O3_S_ma3, df_imp$T_ma3, na.action = na.pass))

#Gases con humedad relativa RH 

#CO_S

plot(df_imp$CO_S_ma3, df_imp$RH_ma3)
cor.test(df_imp$CO_S_ma3, df_imp$RH_ma3)
ccf(df_imp$CO_S_ma3, df_imp$RH_ma3, na.action = na.pass)
print(ccf(df_imp$CO_S_ma3, df_imp$RH_ma3, na.action = na.pass))

#NMHC_S_ma3
plot(df_imp$NMHC_S_ma3, df_imp$RH_ma3)
cor.test(df_imp$NMHC_S_ma3, df_imp$RH_ma3)
ccf(df_imp$NMHC_S_ma3, df_imp$RH_ma3, na.action = na.pass)
print(ccf(df_imp$NMHC_S_ma3, df_imp$RH_ma3, na.action = na.pass))

#NOX_S_ma3

plot(df_imp$NOX_S_ma3, df_imp$RH_ma3)
cor.test(df_imp$NOX_S_ma3, df_imp$RH_ma3)
ccf(df_imp$NOX_S_ma3, df_imp$RH_ma3, na.action = na.pass)
print(ccf(df_imp$NOX_S_ma3, df_imp$RH_ma3, na.action = na.pass))


#NO2_S_ma3

plot(df_imp$NO2_S_ma3, df_imp$RH_ma3)
cor.test(df_imp$NO2_S_ma3, df_imp$RH_ma3)
ccf(df_imp$NO2_S_ma3, df_imp$RH_ma3, na.action = na.pass)
print(ccf(df_imp$NO2_S_ma3, df_imp$RH_ma3, na.action = na.pass))

#O3_S_ma3

plot(df_imp$O3_S_ma3, df_imp$RH_ma3)
cor.test(df_imp$O3_S_ma3, df_imp$RH_ma3)
ccf(df_imp$O3_S_ma3, df_imp$RH_ma3, na.action = na.pass)
print(ccf(df_imp$O3_S_ma3, df_imp$RH_ma3, na.action = na.pass))


#Gases y humedad absoluta AH 

#CO_S

plot(df_imp$CO_S_ma3, df_imp$AH_ma3)
cor.test(df_imp$CO_S_ma3, df_imp$AH_ma3)
ccf(df_imp$CO_S_ma3, df_imp$AH_ma3, na.action = na.pass)
print(ccf(df_imp$CO_S_ma3, df_imp$AH_ma3, na.action = na.pass))

#NMHC_S_ma3
plot(df_imp$NMHC_S_ma3, df_imp$AH_ma3)
cor.test(df_imp$NMHC_S_ma3, df_imp$AH_ma3)
ccf(df_imp$NMHC_S_ma3, df_imp$AH_ma3, na.action = na.pass)
print(ccf(df_imp$NMHC_S_ma3, df_imp$AH_ma3, na.action = na.pass))

#NOX_S_ma3

plot(df_imp$NOX_S_ma3, df_imp$AH_ma3)
cor.test(df_imp$NOX_S_ma3, df_imp$AH_ma3)
ccf(df_imp$NOX_S_ma3, df_imp$AH_ma3, na.action = na.pass)
print(ccf(df_imp$NOX_S_ma3, df_imp$AH_ma3, na.action = na.pass))


#NO2_S_ma3

plot(df_imp$NO2_S_ma3, df_imp$AH_ma3)
cor.test(df_imp$NO2_S_ma3, df_imp$AH_ma3)
ccf(df_imp$NO2_S_ma3, df_imp$AH_ma3, na.action = na.pass)
print(ccf(df_imp$NO2_S_ma3, df_imp$AH_ma3, na.action = na.pass))

#O3_S_ma3

plot(df_imp$O3_S_ma3, df_imp$AH_ma3)
cor.test(df_imp$O3_S_ma3, df_imp$AH_ma3)
ccf(df_imp$O3_S_ma3, df_imp$AH_ma3, na.action = na.pass)
print(ccf(df_imp$O3_S_ma3, df_imp$AH_ma3, na.action = na.pass))

#CO_S versus NO2_S

plot(df_imp$CO_S_ma3, df_imp$NO2_S_ma3)
cor.test(df_imp$CO_S_ma3, df_imp$NO2_S_ma3)
ccf(df_imp$CO_S_ma3, df_imp$NO2_S_ma3, na.action = na.pass)
print(ccf(df_imp$CO_S_ma3, df_imp$NO2_S_ma3, na.action = na.pass))


# MODELO 

#Prueba de estacionariedad (?)

adf.test(df_imp$CO_S_ma3)# ver por NA 
kpss.test(df_imp$CO_S_ma3)

#La mayoria de las ST van a ser no estacionarias 

#quitar la tendencia (filtro pasa altos)
#las bajas F son las tendencias, minimiza la 
#tendencia 






