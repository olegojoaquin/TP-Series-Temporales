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

sapply(df, function(x){sum(is.na(x)/dim(df)[1])})


gases_filtro <- c("CO_S","NMHC_S","NOX_S","NO2_S","O3_S","T","RH","AH" )


#Hago imputacion de media movil
df2 <- df
for(i in gases_filtro){
  df2[paste0(i,'_IMP')]<- na_ma(df2[i], k = 7, weighting = "exponential")
}


#AS TSIBBLE
gases_imp <- c("CO_S_IMP","NMHC_S_IMP","NOX_S_IMP","NO2_S_IMP","O3_S_IMP","T_IMP","RH_IMP","AH_IMP" )

df_g <- df2[c("DATE",gases_imp)]

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

head(df_g)

#Granger causality test
#CO_S_MA3 y NMHC_S
grangertest(CO_S_MA3 ~ NMHC_S_MA3, order = 60, data = df_g) #Order es el nro de lags. El valor default es 1.

#p-valor=2.2e-16 -> menor a 0.05, rechazo la hipotesis nula y se asume que CO_S es relevante a la hora de predecir NMHC_S

#Granger causality test (reverse)
grangertest(NMHC_S_MA3 ~ CO_S_MA3, order = 60, data = df_g)
#p-valor=2.2e-16 -> menor a 0.05, rechazo la hipotesis nula

#CO_S_MA3 y NOX_S_MA3
grangertest(CO_S_MA3 ~ NOX_S_MA3, order = 60, data = df_g)
#p-valor=2.2e-16 -> menor a 0.05, rechazo la hipotesis nula y se asume que CO_S es relevante a la hora de predecir NOX_S

#reverse
grangertest(NOX_S_MA3 ~ CO_S_MA3, order = 60, data = df_g)
#p-valor=2.2e-16 -> menora 0.05, rechazo la hipotesis nula

#CO_S_MA3 y NO2_S_MA3
grangertest(CO_S_MA3 ~ NO2_S_MA3, order = 60, data = df_g)
#p-valor=2.2e-16 -> menor a 0.05, rechazo la hipotesis nula y se asume que CO_S es relevante a la hora de predecir NO2_S

#reverse
grangertest(NO2_S_MA3 ~ CO_S_MA3, order = 60, data = df_g)
#p-valor=2.2e-16 -> menor a 0.05, rechazo la hipotesis nula

#CO_S_MA3 y O3_S_MA3
grangertest(CO_S_MA3 ~ O3_S_MA3, order = 60, data = df_g)
#p-valor=2.2e-16 -> menor a 0.05, rechazo la hipotesis nula y se asume que CO_S es relevante a la hora de predecir O3_S

#reverse
grangertest(O3_S_MA3 ~ CO_S_MA3, order = 60, data = df_g)
#p-valor=2.2e-16 -> menor a 0.05, rechazo la hipotesis nula

#NMHC_S_MA3 y NOX_S_MA3
grangertest(NMHC_S_MA3 ~ NOX_S_MA3, order = 60, data = df_g)
#p-valor=2.2e-16 -> menor a 0.05, rechazo la hipotesis nula y se asume que NMHC_S es relevante a la hora de predecir NOX_S

#reverse
grangertest(NOX_S_MA3 ~ NMHC_S_MA3 , order = 60, data = df_g)
#p-valor=2.2e-16 -> menor a 0.05, rechazo hipotesis nula

#NMHC_S_MA3 y NO2_S_MA3
grangertest(NMHC_S_MA3 ~ NO2_S_MA3, order = 60, data = df_g)
#p-valor=6.14e-09 -> menor a 0.05, rechazo la hipotesis nula y se asume que NMHC_S es relevante a la hora de predecir NO2_S

#reverse
grangertest(NO2_S_MA3 ~ NMHC_S_MA3, order = 60, data = df_g)
#p-valor=2.2e-16 -> menor a 0.05, rechazo la hipotesis nula

#NMHC_S_MA3 y O3_S_MA3
grangertest(NMHC_S_MA3 ~ O3_S_MA3, order = 60, data = df_g)
#p-valor=0.6023 -> mayor a 0.05, no rechazo la hipotesis nula y no se asume que NMHC_S es relevante a la hora de predecir 03_S

#reverse
grangertest(O3_S_MA3 ~ NMHC_S_MA3, order = 60, data = df_g)
#p-valor=0.0001399 -> menor a 0.05, no rechazo la hipotesis nula



