#Null Hypothesis (H0): Time series X does not cause time series Y to Granger-cause itself.
#Alternative Hypothesis (H1): Time series X cause time series Y to Granger-cause itself.

library(lmtest)

setwd("~/Desktop/TP-Series-Temporales-main/")

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

head(df)

#Granger causality test
#CO_S y NMHC_S
grangertest(CO_S ~ NMHC_S, order = 1, data = df) #Order es el nro de lags. El valor default es 1.

#p-valor=2.627e-15 -> menor a 0.05, rechazo la hipotesis nula y se asume que CO_S es relevante a la hora de predecir NMHC_S

#Granger causality test (reverse)
grangertest(NMHC_S ~ CO_S, order = 1, data = df)
#p-valor=8.397e-09 -> menor a 0.05, rechazo la hipotesis nula

#CO_S y NOX_S
grangertest(CO_S ~ NOX_S, order = 1, data = df)
#p-valor=0.2021 -> mayor a 0.05, no rechazo la hipotesis nula y no se asume que CO_S es relevante a la hora de predecir NOX_S

#reverse
grangertest(NOX_S ~ CO_S, order = 1, data = df)
#p-valor=0.5023 -> mayor a 0.05, no rechazo la hipotesis nula

#CO_S y NO2_S
grangertest(CO_S ~ NO2_S, order = 1, data = df)
#p-valor=0.0002251 -> menor a 0.05, rechazo la hipotesis nula y se asume que CO_S es relevante a la hora de predecir NO2_S

#reverse
grangertest(NO2_S ~ CO_S, order = 1, data = df)
#p-valor=2.2e-16 -> menor a 0.05, rechazo la hipotesis nula

#CO_S y O3_S
grangertest(CO_S ~ O3_S, order = 1, data = df)
#p-valor=6.483e-12 -> menor a 0.05, rechazo la hipotesis nula y se asume que CO_S es relevante a la hora de predecir O3_S

#reverse
grangertest(O3_S ~ CO_S, order = 1, data = df)
#p-valor=3.119e-14 -> menor a 0.05, rechazo la hipotesis nula

#NMHC_S y NOX_S
grangertest(NMHC_S ~ NOX_S, order = 1, data = df)
#p-valor=1.177e-08 -> menor a 0.05, rechazo la hipotesis nula y se asume que NMHC_S es relevante a la hora de predecir NOX_S

#reverse
grangertest(NOX_S ~ NMHC_S , order = 1, data = df)
#p-valor=0.0004298 -> menor a 0.05, rechazo hipotesis nula

#NMHC_S y NO2_S
grangertest(NMHC_S ~ NO2_S, order = 1, data = df)
#p-valor=0.0009828 -> menor a 0.05, rechazo la hipotesis nula y se asume que NMHC_S es relevante a la hora de predecir NO2_S

#reverse
grangertest(NO2_S ~ NMHC_S, order = 1, data = df)
#p-valor=2.2e-16 -> menor a 0.05, rechazo la hipotesis nula

#NMHC_S y O3_S
grangertest(NMHC_S ~ O3_S, order = 1, data = df)
#p-valor=0.6023 -> mayor a 0.05, no rechazo la hipotesis nula y no se asume que NMHC_S es relevante a la hora de predecir 03_S

#reverse
grangertest(O3_S ~ NMHC_S, order = 1, data = df)
#p-valor=0.0001399 -> menor a 0.05, no rechazo la hipotesis nula



