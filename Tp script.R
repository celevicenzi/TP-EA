#Bajamos la Data del trabajo practico

library(tseries)
library(urca)
library(ggplot2)
library(forecast)

Data<-read.csv2(file = "Grupo 13.csv")

attach(Data)

Data1<-ts(Data,class = "ts") #Creamos la serie de tiempo de la data. Me mantiene el nombre original

#Graficamos ambas series de tiempo. 
#En primera instancia, la serie A es estacionaria, pero no lo es la serie B
par(mfrow = c(2,1))
plot(ts(Serie_A), main = "Serie A Grupo 13", col = "red") 
plot(ts(Serie_B), main = "Serie B Grupo 13", col = "green")

#Instalar el paquete "psych" para poder utilizar la funcion describe
library(psych)
#ANÁLISIS DESCRIPTIVO#
Analisis.1<-describe(Data1,quant = c(0,0.25,0.5,0.75,1)) #Analisis descriptivo
#skew es la asmimetria. Se encuentran cercanos a 0

#Analizamos la Kurtosis de ambas series
Kurtosis<-kurtosi(Data1) #Ambas son planicurticas

#Varianza
Varianza<-var(Data1)
colnames(Varianza)<-c("Var Serie A", "Var Serie B")

#Unificamos lo calculado hasta ahora
Analisis.Total<-data.frame(Analisis.1,Kurtosis,Varianza) 

#Graficamos el Boxplot
par(mfrow = c(1,2))
boxplot(Serie_A,main = "Box Plot Serie A Grupo 13", col = "red")
boxplot(Serie_B,main = "Box Plot Serie B Grupo 13", col = "green")

#Densidad
par(mfrow = c(1,2))
plot(density(Serie_A),main="Densidad",xlab="N=100",col="red")
plot(density(Serie_B),main="Densidad",xlab="N=100",col="green")

#Histograma
hist(Serie_A, breaks=20, main="Histograma Serie A Grupo 13", col="red")
hist(Serie_B,breaks=20, main="Histograma Serie B Grupo 13", col="green")

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
none.df<-ur.df(Serie_A,type="none",lags=5,selectlags=c("AIC"))
drift.df<-ur.df(Serie_A,type="drift",lags=5,selectlags=c("AIC"))
trend.df<-ur.df(Serie_A,type="trend",lags=5,selectlags=c("AIC"))

#Para los valores criticos
Detalle_none_df<-summary(none.df)
Detalle_drift_df<-summary(drift.df)
Detalle_trend_df<-summary(trend.df)

#Para ver si es estacionario sin tendencia ni termino independiente
for (i in 1:length(Detalle_none_df@cval)) {
  Resultado_none<-none.df@teststat<Detalle_none_df@cval[1,i]
  print(Resultado_none)  
  if (Resultado_none == TRUE) {
    print("Es estacionario")
  }else{
    print("No es estacionario")
  }
}

#Para ver si es estacionario sin tendencia pero con termino independiente
for (i in 1:length(Detalle_drift_df@cval[1,])) {
  Resultado_drift<-drift.df@teststat[1,1]<Detalle_drift_df@cval[1,i]
  print(Resultado_drift)  
  if (Resultado_drift == TRUE) {
    print("Es estacionario")
  }else{
    print("No es estacionario")
  }
}

#Para ver si es estacionario con tendencia y con termino independiente
for (i in 1:length(Detalle_trend_df@cval[1,])) {
  Resultado_trend<-trend.df@teststat[1,1]<Detalle_trend_df@cval[1,i]
  print(Resultado_trend)  
  if (Resultado_trend == TRUE) {
    print("Es estacionario")
  }else{
    print("No es estacionario")
  }
}



#Esto es un intento de obtener los AIC y los BIC de varios modelos ARIMA sin necesidad de hacer 1 por 1. Todavia no esta terminado\

for (i in 0:5) {
  for(j in 0:5){
    if(j!=0 | i!=0){
      m<-arima(Serie_A,order=c(i,0,j))
      Modelo_Serie_A<-cbind(AIC(m),BIC(m))
      rownames(Modelo_Serie_A)<-print(paste("Modelo",i,j))
      colnames(Modelo_Serie_A)<-c("AIC","BIC")
      print(Modelo_Serie_A)
    }
  }
} #Siempre se selecciona el modelos con menor AIC y con menos BIC

#El Modelo MA(3) es el que mejor ajusta, y un modelos MA siempre es estacionario

#Observamos la FAC y la FACP de los residuos del modelo seleccionado
par(mfrow = c(2,2))
plot(acf(MA_3$residuals),main = "Autocorrelacion Modelo MA(3)")
plot(pacf(MA_3$residuals),main = "Autocorrelacion parcial Modelo MA(3)")

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
Lyung_Box_MA_3 #El p-value es mayor a 0.05, por lo tanto no rechazo HO y se puede decir, que no hay autocorrelacion entre los residuos


#Test de Jarque Bera
#La prueba de Jarque-Bera es una prueba de bondad de ajuste para comprobar si una muestra de datos tiene la asimetría y la curtosis de una distribución normal.
#Ho: Los errores tienen una distribucion normal
#H1: no especifica

#Si p-value > alpha. No rechazo Ho

Jarque_Bera_MA_3<-jarque.bera.test(MA_3$residuals)
Jarque_Bera_MA_3

#Ho: Los errores tienen una distribucion normal
#H1: no especifica

#Si p-value > alpha. No rechazo Ho
Test_Shapiro_MA_3<-shapiro.test(MA_3$residuals)
Test_Shapiro_MA_3

#Tanto el test de JB y el Shapiro, se rechaza la Ho de que siguen una distribucion normal

par(mfrow = c(2,2)) #Para poder comparar las distitntas predicciones
#Predicción para un horizonte
Pre1<-forecast(MA_3, level = c(95,97.5,99), h = 1)
plot(Pre1, main = "Prediccion 1 periodo")
P1<-as.data.frame(Pre1)
P1<-data.frame(P1[,6],P1[,4],P1[,2],P1[,1],P1[,3],P1[,5],P1[,7])
colnames(P1)<-c("LI 99%","LI 97,5%","LI 95%","Predicción","LS 95%","LS 97,5%","LS 99%")
P1
#Predicción para dos horizontes
Pre2<-forecast(MA_3, level = c(95,97.5,99), h = 2)
plot(Pre2,main = "Prediccion 2 periodos")
P2<-as.data.frame(Pre2)
P2<-data.frame(P2[,6],P2[,4],P2[,2],P2[,1],P2[,3],P2[,5],P2[,7])
colnames(P2)<-c("LI 99%","LI 97,5%","LI 95%","Predicción","LS 95%","LS 97,5%","LS 99%")
P2
#Predicción para cinco horizontes.
Pre5<-forecast(MA_3, level = c(95,97.5,99), h = 5)
plot(Pre5, main = "Prediccion 5 periodos")
P5<-as.data.frame(Pre5)
P5<-data.frame(P5[,6],P5[,4],P5[,2],P5[,1],P5[,3],P5[,5],P5[,7])
colnames(P5)<-c("LI 99%","LI 97,5%","LI 95%","Predicción","LS 95%","LS 97,5%","LS 99%")
P5
#Predicción para diez horizontes.
Pre10<-forecast(MA_3, level = c(95,97.5,99), h = 10)
plot(Pre10, main = "Prediccion 10 periodos")
P10<-as.data.frame(Pre10)
P10<-data.frame(P10[,6],P10[,4],P10[,2],P10[,1],P10[,3],P10[,5],P10[,7])
colnames(P10)<-c("LI 99%","LI 97,5%","LI 95%","Predicción","LS 95%","LS 97,5%","LS 99%")
rownames(P10)<-c(1:10)


par(mfrow=c(2,2))
plot(MA_3$residuals,col="red",main="Residuos MA(3)")
hist(MA_3$residuals,main="Histograma de los residuos")
acf(MA_3$residuals,col="red",main="FAC de residuos")
pacf(MA_3$residuals,col="red",main="FACP de Residuos")







#Prueba de Dickey-Fuller Aumentado Serie B
none.df<-ur.df(Serie_B_Diff,type="none",lags=5,selectlags=c("AIC"))
drift.df<-ur.df(Serie_B_Diff,type="drift",lags=5,selectlags=c("AIC"))
trend.df<-ur.df(Serie_B_Diff,type="trend",lags=5,selectlags=c("AIC"))

summary(none.df)
summary(drift.df)
summary(trend.df)

adf.test(Serie_B_Diff)

par(mfrow = c(2,1))
plot(ts(Serie_A), main = "Serie A Grupo 13", col = "red")
plot(ts(Serie_B_Diff), main = "Serie B Dif Grupo 13", col = "green") #Parece ser estacionaria por el momento

#Se compara nuevamente los box plot
par(mfrow = c(1,2))
boxplot(Serie_A,main = "Box Plot Serie A Grupo 13", col = "red")
boxplot(Serie_B_Diff,main = "Box Plot Serie B Dif Grupo 13", col = "green")

#Se vuelve a graficar la FAS, FAC y FACP
par(mfrow = c(3,2))
acf(Serie_A, main = "FAC Serie A", col = "red")
acf(Serie_B_Diff, main = "FAC Serie B Dif", col = "green")
pacf(Serie_A, main = "FACP Serie A", col = "red") 
pacf(Serie_B_Diff, main = "FACP Serie B Dif", col = "green") #Ahora si parece ser estacionario  


-#Modelizamos ---> Primero hay que testear
#Hay una forma simple de hacerla, que es con una funcion de AUTO.ARIMA, que nos dice el mejor modelo. Pero para comparar, se va a hacer completo

auto.arima(Serie_A,stationary = TRUE) #stationary TRUE para que me indique modelos estacionarioa 
#El modelo nos da un MA(3), y una MA siempre es estacionario.

M1_SerieA<-arima(Serie_A,order = c(1,0,0))
M1_SerieA

M2_SerieA<-arima(Serie_A,order = c(0,0,1))
M2_SerieA

M3_SerieA<-arima(Serie_A,order = c(1,0,1))
M3_SerieA

M3_SerieA<-arima(Serie_A,order = c(2,0,1))
M3_SerieA

M4_SerieA<-arima(Serie_A,order = c(1,0,2))
M4_SerieA

M5_SerieA<-arima(Serie_A,order = c(2,0,2))
M5_SerieA

M6_SerieA<-arima(Serie_A,order = c(3,0,0))
M6_SerieA

M7_SerieA<-arima(Serie_A,order = c(3,0,1))
M7_SerieA

M8_SerieA<-arima(Serie_A,order = c(0,0,3))
M8_SerieA

M9_SerieA<-arima(Serie_A,order = c(1,0,3))
M9_SerieA

M10_SerieA<-arima(Serie_A,order = c(3,0,3))
M10_SerieA

M11_SerieA<-arima(Serie_A,order = c(1,1,1))
M11_SerieA

M12_SerieA<-arima(Serie_A,order = c(2,1,1))
M12_SerieA

M13_SerieA<-arima(Serie_A,order = c(1,1,2))
M13_SerieA

M14_SerieA<-arima(Serie_A,order = c(2,1,2))
M14_SerieA

Serie_A_aic<-rbind(AIC(M1_SerieA,M2_SerieA,M3_SerieA,M4_SerieA,M5_SerieA,M6_SerieA,M7_SerieA,M8_SerieA,M9_SerieA,M10_SerieA),AIC(M11_SerieA,M12_SerieA,M13_SerieA,M14_SerieA))
Serie_A_bic<-rbind(BIC(M1_SerieA,M2_SerieA,M3_SerieA,M4_SerieA,M5_SerieA,M6_SerieA,M7_SerieA,M8_SerieA,M9_SerieA,M10_SerieA),BIC(M11_SerieA,M12_SerieA,M13_SerieA,M14_SerieA))

Modelo_Serie_A<-cbind(Serie_A_aic,Serie_A_bic)
Modelo_Serie_A
M8_SerieA

#Observamos la FAC y la FACP de los residuos del modelo seleccionado
par(mfrow = c(2,2))
plot(acf(M8_SerieA$residuals))
plot(pacf(M8_SerieA$residuals))

#Hacemos analisis del modelo selecionado

#Test sobre los coeficientes del modelo
#Ho: Algun tita i es = 0
#H1: Los tita i distintos de 0
Test_Coef_M8<-t.test(M8_SerieA$coef)
Test_Coef_M8 #El test, indica que la H1 es verdadera, por lo tanto los coeficientes son distintos de 0


#Test de Ljung=Box
#Utilice el estadístico q de Ljung-Box para comprobar si una serie de observaciones en un período de tiempo específico son aleatorias e independientes.
#...Si las observaciones no son independientes, una observación puede estar correlacionada con otra observación k unidades de tiempo después,... 
#...una relación que se denomina autocorrelación.

#Ho: las autocorrelaciones son iguales a 0. Es decir, los datos son independientes
#H1: No todos los datos son independientes

#Si chi-cuadrado > alpha. No rechazo Ho

Lyung_Box_Serie_A<-Box.test(M8_SerieA$residuals,type = "Ljung-Box",lag = 1)
Lyung_Box_Serie_A #El p-value es mayor a 0.05, por lo tanto no rechazo HO y se puede decir, que no hay autocorrelacion entre los residuos
