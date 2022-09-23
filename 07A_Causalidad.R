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

#######################
# 1 REGISTRO POR DIA  #
#######################


df_g <- df3 %>%  group_by(DATE) %>%  summarise (across(everything(), mean))

df_g$DATE <- as.Date(as.character(df_g$DATE),format="%Y-%m-%d")

df_g <- df_g  %>% as_tsibble(index= DATE)

head(df_g)

#Granger causality test
#CO_S y NMHC_S
grangertest(CO_S_IMP ~ NMHC_S_IMP, order = 1, data = df_g) #Order es el nro de lags. El valor default es 1.

#p-valor=0.1935 -> mayora 0.05, no rechazo la hipotesis nula y se asume que CO_S es relevante a la hora de predecir NMHC_S

#Granger causality test (reverse)
grangertest(NMHC_S_IMP ~ CO_S_IMP, order = 1, data = df_g)
#p-valor=0.01431 -> menor a 0.05, rechazo la hipotesis nula

#CO_S y NOX_S
grangertest(CO_S_IMP ~ NOX_S_IMP, order = 1, data = df_g)
#p-valor=0.02843 -> Menor a 0.05, rechazo la hipotesis nula y no se asume que CO_S es relevante a la hora de predecir NOX_S

#reverse
grangertest(NOX_S_IMP ~ CO_S_IMP, order = 1, data = df_g)
#p-valor=0.3834 -> mayor a 0.05, no rechazo la hipotesis nula


#CO_S y NO2_S
grangertest(CO_S_IMP ~ NO2_S_IMP, order = 1, data = df_g)
#p-valor=0.01066 -> menor a 0.05, rechazo la hipotesis nula y se asume que CO_S es relevante a la hora de predecir NO2_S

#reverse
grangertest(NO2_S_IMP ~ CO_S_IMP, order = 1, data = df_g)
#p-valor=2.011e-05-> menor a 0.05, rechazo la hipotesis nula


#CO_S y O3_S
grangertest(CO_S_IMP ~ O3_S_IMP, order = 1, data = df_g)
#p-valor=0.01315  -> menor a 0.05, rechazo la hipotesis nula y se asume que CO_S es relevante a la hora de predecir O3_S

#reverse
grangertest(O3_S_IMP ~ CO_S_IMP, order = 1, data = df_g)
#p-valor=0.004023 -> menor a 0.05, rechazo la hipotesis nula


#NMHC_S y NOX_S
grangertest(NMHC_S_IMP ~ NOX_S_IMP, order = 1, data = df_g)
#p-valor=0.006218 -> menor a 0.05, rechazo la hipotesis nula y se asume que NMHC_S es relevante a la hora de predecir NOX_S

#reverse
grangertest(NOX_S_IMP ~ NMHC_S_IMP , order = 1, data = df_g)
#p-valor=0.2094 -> menor a 0.05, rechazo hipotesis nula

#NMHC_S y NO2_S
grangertest(NMHC_S_IMP ~ NO2_S_IMP, order = 1, data = df_g)
#p-valor=0.7404 -> mayor a 0.05, no rechazo la hipotesis nula y se asume que NMHC_S no es relevante a la hora de predecir NO2_S

#reverse
grangertest(NO2_S_IMP ~ NMHC_S_IMP, order = 1, data = df_g)
#p-valor=0.007875 -> menor a 0.05, rechazo la hipotesis nula


#NMHC_S y O3_S
grangertest(NMHC_S_IMP ~ O3_S_IMP, order = 1, data = df_g)
#p-valor=0.3773 -> mayor a 0.05, no rechazo la hipotesis nula y no se asume que NMHC_S es relevante a la hora de predecir 03_S

#reverse
grangertest(O3_S_IMP ~ NMHC_S_IMP, order = 1, data = df_g)
#p-valor=0.8523 -> mayor a 0.05, no rechazo la hipotesis nula



###########################
#CAUSAL IMPACT
###########################

library(CausalImpact)

matplot(df_g[,2:9], type = "l")

#Uso un ejemplo con causalidad CO_S_MA3 ~ NMHC_S_MA3
data <- df_g[c("DATE","CO_S_MA3","NMHC_S_MA3")]

pre.period <- as.Date(c("2004-03-10", "2005-02-15"))
post.period <- as.Date(c("2005-02-16", "2005-04-04"))

impact <- CausalImpact(data, pre.period, post.period)
plot(impact)
summary(impact)

#Uso un ejemplo sin causalidad NMHC_S_MA3 ~ O3_S_MA3
data1 <- df_g[c("DATE","NMHC_S_MA3","O3_S_MA3")]

pre.period <- as.Date(c("2004-03-10", "2005-02-15"))
post.period <- as.Date(c("2005-02-16", "2005-04-04"))

impact1 <- CausalImpact(data1, pre.period, post.period)
plot(impact1)
summary(impact1)







