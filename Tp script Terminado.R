#Instalamos los paquetes necesarios que vamos a utilizar

#install.packages("tseries")
#install.packages("urca")
#install.packages("ggplot2")
#install.packages("forecast")
#install.packages("plotly")


#Los bajamos al proyecto en el cual trabajaremos
library(tseries)
library(urca)
library(ggplot2)
library(forecast)
library(plotly)

#Bajamos la Data del trabajo practico
Data<-read.csv2(file = "Grupo 13.csv")

attach(Data)


Data1<-ts(Data,class = "ts") #Creamos la serie de tiempo de la data. Me mantiene el nombre original

#Graficamos ambas series de tiempo. 
par(mfrow = c(2,1))
plot(ts(Serie_A), main = "Serie A Grupo 13", col = "red") 
plot(ts(Serie_B), main = "Serie B Grupo 13", col = "green")
#En primera instancia, la serie A es estacionaria, pero no lo es la serie B



#install.packages("psych")
library(psych)

#ANÁLISIS DESCRIPTIVO#
Analisis.1<-describe(Data1,quant = c(0,0.25,0.5,0.75,1)) #Analisis descriptivo
Analisis.1
#skew es la asmimetria. Se encuentran cercanos a 0

#Varianza
Varianza<-var(Data1)
colnames(Varianza)<-c("Var Serie A", "Var Serie B")
Covarianza=matrix(c(Varianza[1,2],Varianza[2,1]),ncol=1,nrow=2)
Varianza=matrix(c(Varianza[1,1],Varianza[2,2]),ncol=1,nrow=2)


#Unificamos lo calculado hasta ahora
Analisis.Total<-data.frame(Analisis.1,Varianza,Covarianza) 
Analisis.Total
#Cosas a considerar para el analisis: Media, desvio, varianza, simetria, kurtosis.

#Graficamos el Boxplot
par(mfrow = c(1,2))
boxplot(Serie_A,main = "Box Plot Serie A Grupo 13", col = "red")
boxplot(Serie_B,main = "Box Plot Serie B Grupo 13", col = "green")
#Observamos que la media se encuentra casi en el centro en la serie A, pero no en la serie B. En ambos casos, no hay valores atipicos

#Densidad
par(mfrow = c(1,1))
plot(density(Serie_B),main="Densidad",xlab="N=100",col="green")
lines(density(Serie_A), col="red")
#En la serie A, no parece haber normalidad, pero si en la serie B

#Histograma
par(mfrow = c(1,2))
hist(Serie_A, breaks=20, main="Histograma Serie A Grupo 13", col="red")
hist(Serie_B,breaks=20, main="Histograma Serie B Grupo 13", col="green")
#En la serie A, no parece haber normalidad, pero si en la serie B, al igual que en la densidad


#FAS, FAC y FACP
par(mfrow = c(4,2))
plot(ts(Serie_A), main = "Serie A Grupo 13", col = "red") 
plot(ts(Serie_B), main = "Serie B Grupo 13", col = "green")
acf(Serie_A, type="covariance", main="FAS Serie A", col="red")
acf(Serie_B, type="covariance", main="FAS Serie B", col="green")
acf(Serie_A, main = "FAC Serie A", col = "red")
acf(Serie_B, main = "FAC Serie B", col = "green")
pacf(Serie_A, main = "FACP Serie A", col = "red") 
pacf(Serie_B, main = "FACP Serie B", col = "green") 
#Otra vez, en primera instancia la serie B no es estacionaria



##### SERIE A #####
auto.arima(Serie_A, stepwise = FALSE, approximation = FALSE)
#Segun auto.arima, el mejor modelo es una MA(3)
ndiffs(Serie_A)
#Segun ndiffs, no hay que diferenciar la serie

#Test de Dickey Fuller
#Para el estadistico de prueba
none.df_A<-ur.df(Serie_A,type="none",lags=5,selectlags=c("AIC"))
drift.df_A<-ur.df(Serie_A,type="drift",lags=5,selectlags=c("AIC"))
trend.df_A<-ur.df(Serie_A,type="trend",lags=5,selectlags=c("AIC"))

#Para los valores criticos
Detalle_none_df_A<-summary(none.df_A)
Detalle_drift_df_A<-summary(drift.df_A)
Detalle_trend_df_A<-summary(trend.df_A)


#Las siguientes funciones, comparan el estadistico de pruba, contra los TAU correspondientes a distintos niveles de significancia
#Para ver si es estacionario sin tendencia ni termino independiente
for (i in 1:length(Detalle_none_df_A@cval)) {
  Resultado_none_A<-none.df_A@teststat<Detalle_none_df_A@cval[1,i]
  print(Resultado_none_A)  
  if (Resultado_none_A == TRUE) {
    print("Es estacionario")
  }else{
    print("No es estacionario")
  }
}

#Para ver si es estacionario sin tendencia pero con termino independiente
for (i in 1:length(Detalle_drift_df_A@cval[1,])) {
  Resultado_drift_A<-drift.df_A@teststat[1,1]<Detalle_drift_df_A@cval[1,i]
  print(Resultado_drift_A)  
  if (Resultado_drift_A == TRUE) {
    print("Es estacionario")
  }else{
    print("No es estacionario")
  }
}

#Para ver si es estacionario con tendencia y con termino independiente
for (i in 1:length(Detalle_trend_df_A@cval[1,])) {
  Resultado_trend_A<-trend.df_A@teststat[1,1]<Detalle_trend_df_A@cval[1,i]
  print(Resultado_trend_A)  
  if (Resultado_trend_A == TRUE) {
    print("Es estacionario")
  }else{
    print("No es estacionario")
  }
}



#Para obtener los AIC y los BIC de varios modelos ARIMA sin necesidad de hacer 1 por 1. 
#for (i in 0:5) {
  #for(j in 0:5){
    #if(j!=0 | i!=0){
      #m<-arima(Serie_A,order=c(i,0,j))
      #Modelo_Serie_A<-cbind(AIC(m),BIC(m))
      #rownames(Modelo_Serie_A)<-print(paste("Modelo",i,j))
      #colnames(Modelo_Serie_A)<-c("AIC","BIC")
      #print(Modelo_Serie_A)
    #}
  #}
#} #Siempre se selecciona el modelos con menor AIC y con menos BIC

MA_3<-auto.arima(Serie_A, stepwise = FALSE, approximation = FALSE,trace = TRUE) #TRACE devuelve los AIC de todos los modelos

#El Modelo MA(3) es el que mejor ajusta, y un modelos MA siempre es estacionario


#Observamos la FAC y la FACP de los residuos del modelo seleccionado
par(mfrow = c(2,2))
plot(acf(MA_3$residuals),main = "Autocorrelacion Modelo MA(3)",col = "red")
plot(pacf(MA_3$residuals),main = "Autocorrelacion parcial Modelo MA(3)",col = "red")

#Hacemos analisis del modelo selecionado

#Test sobre los coeficientes del modelo
#Ho: Algun tita i es = 0
#H1: Los tita i distintos de 0
Test_Coef_MA_3<-t.test(MA_3$coef)
Test_Coef_MA_3 #El test, indica que la H1 es verdadera, por lo tanto los coeficientes son distintos de 0


#Test de Ljung=Box
#Utilice el estadístico q de Ljung-Box para comprobar si una serie de observaciones en un período de tiempo específico son aleatorias e independientes.
#...Si las observaciones no son independientes, una observación puede estar correlacionada con otra observación k unidades de tiempo después,... 
#...una relación que se denomina autocorrelación.

#Ho: las autocorrelaciones son iguales a 0. Es decir, los datos son independientes
#H1: No todos los datos son independientes

#Si p-value > alpha. No rechazo Ho

Lyung_Box_MA_3<-Box.test(MA_3$residuals,type = "Ljung-Box",lag = 1)
if (Lyung_Box_MA_3$p.value>0.05) {
  print("No se rechaza la Ho, y por lo tanto no hay autocorrelacion entre los residuos")
}else{
  print("Se rechaza la Ho, y por lo tanto hay autocorrelacion entre los residuos")
}

#Test de Jarque Bera
#La prueba de Jarque-Bera es una prueba de bondad de ajuste para comprobar si una muestra de datos tiene la asimetría y la curtosis de una distribución normal.
#Ho: Los errores tienen una distribucion normal
#H1: no especifica

#Si p-value > alpha. No rechazo Ho

Jarque_Bera_MA_3<-jarque.bera.test(MA_3$residuals)
if (Jarque_Bera_MA_3$p.value>0.05) {
  print("No se rechaza la Ho, y por lo tanto hay normalidad en los residuos")
}else{
  print("Se rechaza la Ho, y por lo tanto los residuos tienen una distribucion distinta a la normal")
}

#Test de Shapiro
#Ho: Los errores tienen una distribucion normal
#H1: no especifica

#Si p-value > alpha. No rechazo Ho
Test_Shapiro_MA_3<-shapiro.test(MA_3$residuals)
if (Test_Shapiro_MA_3$p.value>0.05) {
  print("No se rechaza la Ho, y por lo tanto hay normalidad en los residuos")
}else{
  print("Se rechaza la Ho, y por lo tanto los residuos tienen una distribucion distinta a la normal")
}

#Tanto el test de JB y el Shapiro, se rechaza la Ho de que siguen una distribucion normal. Lo podes observar si volvemos a ver el grafico de densidad y el histograma
par(mfrow = c(1,2))
hist(Serie_A, breaks=20, main="Histograma Serie A ", col="red")
plot(density(Serie_A),main="Densidad",xlab="N=100",col="red")

transformacionSerieA <- BoxCox(Serie_A, lambda = "auto")
modeloTransformadoA <- auto.arima(transformacionSerieA, stationary = T)
jarque.bera.test(modeloTransformadoA$residuals)
#La transformación de Box Cox con lambda automatizado tampoco genera un modelo con residuos de distribución normal.

par(mfrow = c(2,2)) #Para poder comparar las distitntas predicciones
#Predicción para un horizonte
Pre1<-forecast(MA_3, level = c(94,95,99), h = 1) #Prediccion
plot(Pre1, main = "Prediccion 1 periodo") #Grafico
P1<-as.data.frame(Pre1) #Se hace data frame
P1<-data.frame(P1[,6],P1[,4],P1[,2],P1[,1],P1[,3],P1[,5],P1[,7]) #Se ordenen las columnas
colnames(P1)<-c("LI 99%","LI 95%","LI 94%","Predicción","LS 94%","LS 95%","LS 99%") #Se les asignan nuevos nombres para que sea entendible
P1
#Predicción para dos horizontes
Pre2<-forecast(MA_3, level = c(94,95,99), h = 2)
plot(Pre2,main = "Prediccion 2 periodos")
P2<-as.data.frame(Pre2)
P2<-data.frame(P2[,6],P2[,4],P2[,2],P2[,1],P2[,3],P2[,5],P2[,7])
colnames(P2)<-c("LI 99%","LI 95%","LI 94%","Predicción","LS 94%","LS 95%","LS 99%")
P2
#Predicción para tres horizontes.
Pre3<-forecast(MA_3, level = c(94,95,99), h = 3)
plot(Pre3, main = "Prediccion 3 periodos")
P3<-as.data.frame(Pre3)
P3<-data.frame(P3[,6],P3[,4],P3[,2],P3[,1],P3[,3],P3[,5],P3[,7])
colnames(P3)<-c("LI 99%","LI 95%","LI 94%","Predicción","LS 94%","LS 95%","LS 99%")
P3
#Predicción para 20 horizontes.
Pre20<-forecast(MA_3, level = c(94,95,99), h = 20)
plot(Pre20, main = "Prediccion 20 periodos")
P20<-as.data.frame(Pre20)
P20<-data.frame(P20[,6],P20[,4],P20[,2],P20[,1],P20[,3],P20[,5],P20[,7])
colnames(P20)<-c("LI 99%","LI 95%","LI 94%","Predicción","LS 94%","LS 95%","LS 99%")
rownames(P20)<-c(1:20)
P20

#Puede predecir hasta 3 periodos, porque se tiene un MA(3), y no tiene datos suficientes para continuar la prediccion, a menos que se utilice los datos antesriores obtenidos

write.csv2(P1,file = "Prediccion Serie A 1 periodo")
write.csv2(P2,file = "Prediccion Serie A 2 periodo")
write.csv2(P3,file = "Prediccion Serie A 3 periodo")
write.csv2(P20,file = "Prediccion Serie A 20 periodo")

par(mfrow=c(2,2))
plot(MA_3$residuals,col="red",main="Residuos MA(3)")
hist(MA_3$residuals,main="Histograma de los residuos")
acf(MA_3$residuals,col="red",main="FAC de residuos")
pacf(MA_3$residuals,col="red",main="FACP de Residuos")


##### SERIE B #####
auto.arima(Serie_B, stepwise = FALSE, approximation = FALSE,trace = TRUE)
#Segun auto.arima, el mejor modelo es una ARIMA(0,1,1)
ndiffs(Serie_B)
#Segun ndiffs, hay que diferenciar la serie 1 vez

Serie_B_Diff<-diff(Serie_B)

#Test de Dickey Fuller
#Para el estadistico de prueba
none.df_B_Diff<-ur.df(Serie_B_Diff,type="none",lags=5,selectlags=c("AIC"))
drift.df_B_Diff<-ur.df(Serie_B_Diff,type="drift",lags=5,selectlags=c("AIC"))
trend.df_B_Diff<-ur.df(Serie_B_Diff,type="trend",lags=5,selectlags=c("AIC"))

#Para los valores criticos
Detalle_none_df_B_Diff<-summary(none.df_B_Diff)
Detalle_drift_df_B_Diff<-summary(drift.df_B_Diff)
Detalle_trend_df_B_Diff<-summary(trend.df_B_Diff)

#Para ver si es estacionario sin tendencia ni termino independiente
for (i in 1:length(Detalle_none_df_B_Diff@cval)) {
  Resultado_none_B_Diff<-none.df_B_Diff@teststat<Detalle_none_df_B_Diff@cval[1,i]
  print(Resultado_none_B_Diff)  
  if (Resultado_none_B_Diff == TRUE) {
    print("Es estacionario")
  }else{
    print("No es estacionario")
  }
}

#Para ver si es estacionario sin tendencia pero con termino independiente
for (i in 1:length(Detalle_drift_df_B_Diff@cval[1,])) {
  Resultado_drift_B_Diff<-drift.df_B_Diff@teststat[1,1]<Detalle_drift_df_B_Diff@cval[1,i]
  print(Resultado_drift_B_Diff)  
  if (Resultado_drift_B_Diff == TRUE) {
    print("Es estacionario")
  }else{
    print("No es estacionario")
  }
}

#Para ver si es estacionario con tendencia y con termino independiente
for (i in 1:length(Detalle_trend_df_B_Diff@cval[1,])) {
  Resultado_trend_B_Diff<-trend.df_B_Diff@teststat[1,1]<Detalle_trend_df_B_Diff@cval[1,i]
  print(Resultado_trend_B_Diff)  
  if (Resultado_trend_B_Diff == TRUE) {
    print("Es estacionario")
  }else{
    print("No es estacionario")
  }
}



#Esto es un intento de obtener los AIC y los BIC de varios modelos ARIMA sin necesidad de hacer 1 por 1. Todavia no esta terminado\

#for (i in 0:5) {
  #for(j in 0:5){
    #if(j!=0 | i!=0){
      #n<-arima(Serie_B_Diff,order=c(i,1,j))
      #Modelo_Serie_B_Diff<-cbind(round(AIC(n),2),round(BIC(n),2))
      #rownames(Modelo_Serie_B_Diff)<-print(paste("Modelo",i,j))
      #colnames(Modelo_Serie_B_Diff)<-c("AIC","BIC")
      #print(Modelo_Serie_B_Diff)
    #}
  #}
#} #Siempre se selecciona el modelos con menor AIC y con menos BIC

ARIMA_001<-auto.arima(Serie_B_Diff,stepwise = FALSE, approximation = FALSE,trace = TRUE)

#El Modelo ARIMA(0,0,1) es el que mejor ajusta, y un modelo MA siempre es estacionario

#Observamos la FAC y la FACP de los residuos del modelo seleccionado
par(mfrow = c(2,2))
plot(acf(ARIMA_001$residuals),main = "Autocorrelacion Modelo ARIMA(0,1,1)",col = "green")
plot(pacf(ARIMA_001$residuals),main = "Autocorrelacion parcial Modelo ARIMA(0,1,1)",col = "green")

#Hacemos analisis del modelo selecionado

#Test sobre los coeficientes del modelo
#Ho: Algun tita i es = 0
#H1: Los tita i distintos de 0
Test_Coef_ARIMA_001<-t.test(ARIMA_001$coef)
Test_Coef_ARIMA_001 #El test, indica que la H1 es verdadera, por lo tanto los coeficientes son distintos de 0


#Test de Ljung=Box
#Utilice el estadístico q de Ljung-Box para comprobar si una serie de observaciones en un período de tiempo específico son aleatorias e independientes.
#...Si las observaciones no son independientes, una observación puede estar correlacionada con otra observación k unidades de tiempo después,... 
#...una relación que se denomina autocorrelación.

#Ho: las autocorrelaciones son iguales a 0. Es decir, los datos son independientes
#H1: No todos los datos son independientes

#Si p-value > alpha. No rechazo Ho

Lyung_Box_ARIMA_001<-Box.test(ARIMA_001$residuals,type = "Ljung-Box",lag = 1)
if (Lyung_Box_ARIMA_001$p.value>0.05) {
  print("No se rechaza la Ho, y por lo tanto no hay autocorrelacion entre los residuos")
}else{
  print("Se rechaza la Ho, y por lo tanto hay autocorrelacion entre los residuos")
}

#Test de Jarque Bera
#La prueba de Jarque-Bera es una prueba de bondad de ajuste para comprobar si una muestra de datos tiene la asimetría y la curtosis de una distribución normal.
#Ho: Los errores tienen una distribucion normal
#H1: no especifica

#Si p-value > alpha. No rechazo Ho

Jarque_Bera_ARIMA_001<-jarque.bera.test(ARIMA_001$residuals)
if (Jarque_Bera_ARIMA_001$p.value>0.05) {
  print("No se rechaza la Ho, y por lo tanto hay normalidad en los residuos")
}else{
  print("Se rechaza la Ho, y por lo tanto los residuos tienen una distribucion distinta a la normal")
}

#Test de Shapiro
#Ho: Los errores tienen una distribucion normal
#H1: no especifica

#Si p-value > alpha. No rechazo Ho
Test_Shapiro_ARIMA_001<-shapiro.test(ARIMA_001$residuals)
if (Test_Shapiro_ARIMA_001$p.value>0.05) {
  print("No se rechaza la Ho, y por lo tanto hay normalidad en los residuos")
}else{
  print("Se rechaza la Ho, y por lo tanto los residuos tienen una distribucion distinta a la normal")
}

par(mfrow = c(2,2)) #Para poder comparar las distitntas predicciones
#Predicción para un horizonte
Pre1_B_Diff<-forecast(ARIMA_001, level = c(94,95,99), h = 1)
plot(Pre1_B_Diff, main = "Prediccion 1 periodo")
P1_B_Diff<-as.data.frame(Pre1_B_Diff)
P1_B_Diff<-data.frame(P1_B_Diff[,6],P1_B_Diff[,4],P1_B_Diff[,2],P1_B_Diff[,1],P1_B_Diff[,3],P1_B_Diff[,5],P1_B_Diff[,7])
colnames(P1_B_Diff)<-c("LI 99%","LI 95%","LI 94%","Predicción","LS 94%","LS 95%","LS 99%")
P1_B_Diff
#Predicción para dos horizontes
Pre2_B_Diff<-forecast(ARIMA_001, level = c(94,95,99), h = 2)
plot(Pre2_B_Diff,main = "Prediccion 2 periodos")
P2_B_Diff<-as.data.frame(Pre2_B_Diff)
P2_B_Diff<-data.frame(P2_B_Diff[,6],P2_B_Diff[,4],P2_B_Diff[,2],P2_B_Diff[,1],P2_B_Diff[,3],P2_B_Diff[,5],P2_B_Diff[,7])
colnames(P2_B_Diff)<-c("LI 99%","LI 95%","LI 94%","Predicción","LS 94%","LS 95%","LS 99%")
P2_B_Diff
#Predicción para tres horizontes.
Pre3_B_Diff<-forecast(ARIMA_001, level = c(94,95,99), h = 3)
plot(Pre3_B_Diff, main = "Prediccion 3 periodos")
P3_B_Diff<-as.data.frame(Pre3_B_Diff)
P3_B_Diff<-data.frame(P3_B_Diff[,6],P3_B_Diff[,4],P3_B_Diff[,2],P3_B_Diff[,1],P3_B_Diff[,3],P3_B_Diff[,5],P3_B_Diff[,7])
colnames(P3_B_Diff)<-c("LI 99%","LI 95%","LI 94%","Predicción","LS 94%","LS 95%","LS 99%")
P3_B_Diff
#Predicción para 20 horizontes.
Pre20_B_Diff<-forecast(ARIMA_001, level = c(94,95,99), h = 20)
plot(Pre20_B_Diff, main = "Prediccion 20 periodos")
P20_B_Diff<-as.data.frame(Pre20_B_Diff)
P20_B_Diff<-data.frame(P20_B_Diff[,6],P20_B_Diff[,4],P20_B_Diff[,2],P20_B_Diff[,1],P20_B_Diff[,3],P20_B_Diff[,5],P20_B_Diff[,7])
colnames(P20_B_Diff)<-c("LI 99%","LI 95%","LI 94%","Predicción","LS 94%","LS 95%","LS 99%")
rownames(P20_B_Diff)<-c(1:20)
P20_B_Diff

write.csv2(P1,file = "Prediccion Serie B Diferenciada 1 periodo")
write.csv2(P2,file = "Prediccion Serie B Diferenciada 2 periodo")
write.csv2(P3,file = "Prediccion Serie B Diferenciada 3 periodo")
write.csv2(P20,file = "Prediccion Serie B Diferenciada 20 periodo")

par(mfrow=c(2,2))
plot(ARIMA_001$residuals,col="red",main="Residuos MA(3)")
hist(ARIMA_001$residuals,main="Histograma de los residuos")
acf(ARIMA_001$residuals,col="red",main="FAC de residuos")
pacf(ARIMA_001$residuals,col="red",main="FACP de Residuos")
