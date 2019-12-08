#Bajamos la Data del trabajo practico

library(tseries)
library(urca)

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
describe(Data,quant = c(0,0.25,0.5,0.75,1)) 
#skew es la asmimetria. Se encuentran cercanos a 0

#Analizamos la Kurtosis de ambas series
kurtosi(Data) #Ambas son planicurticas

#Graficamos el Boxplot
par(mfrow = c(1,2))
boxplot(Serie_A,main = "Box Plot Serie A Grupo 13", col = "red")
boxplot(Serie_B,main = "Box Plot Serie B Grupo 13", col = "green")

#Densidad
plot(density(Serie_A),main="Densidad",xlab="N=100",col="red")
plot(density(Serie_B),main="Densidad",xlab="N=100",col="green")

#Histograma
hist(Serie_A, breaks=20, main="Histograma Serie A Grupo 13", col="red")
hist(Serie_B,breaks=20, main="Histograma Serie B Grupo 13", col="green")

#FAS, FAC y FACP
par(mfrow = c(2,2))
plot(ts(Serie_A), main = "Serie A Grupo 13", col = "red") 
plot(ts(Serie_B), main = "Serie B Grupo 13", col = "green")
acf(Serie_A, type="covariance", main="FAS Serie A", col="red")
acf(Serie_B, type="covariance", main="FAS Serie B", col="green")
acf(Serie_A, main = "FAC Serie A", col = "red")
acf(Serie_B, main = "FAC Serie B", col = "green")
pacf(Serie_A, main = "FACP Serie A", col = "red") 
pacf(Serie_B, main = "FACP Serie B", col = "green") 
#Otra vez, en primera instancia la serie B no es estacionaria

#Prueba de estacionariedad
library(tseries)
#Diferenciamos las series
Serie_A_Diff<-diff(Serie_A)
Serie_B_Diff<-diff(Serie_B) 
#Prueba de Dickey-Fuller Aumentado Serie A
none.df<-ur.df(Serie_A_Diff,type="none",lags=5,selectlags=c("AIC"))
drift.df<-ur.df(Serie_A_Diff,type="drift",lags=5,selectlags=c("AIC"))
trend.df<-ur.df(Serie_A_Diff,type="trend",lags=5,selectlags=c("AIC"))

summary(none.df)
summary(drift.df)
summary(trend.df)

adf.test(Serie_A_Diff)

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
