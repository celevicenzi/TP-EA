#Bajamos la Data del trabajo practico

library(tseries)
library(urca)

Data<-read.csv2(file = "Grupo 13.csv")

attach(Data)

Data1<-ts(Data,class = "ts") #Creamos la serie de tiempo de la data. Me mantiene el nombre original

par(mfrow = c(2,1))
plot(ts(Serie_A), main = "Serie A Grupo 13", col = "red") #Graficamos ambas series de tiempo. En primera instancia, la serie A es estacionaria, pero no lo es la serie B
plot(ts(Serie_B), main = "Serie B Grupo 13", col = "green")
#Instalar el paquete "psych" para poder utilizar la funcion describe

library(psych)

describe(Data,quant = c(0,0.25,0.5,0.75,1)) #Analisis descriptivo
#skew es la asmimetria. Se encuentran cercanos a 0

#Analizamos la Kurtosis de ambas series

kurtosi(Data) #Ambas son planicurticas

#Graficamos el Boxplot
par(mfrow = c(1,2))
boxplot(Serie_A,main = "Box Plot Serie A Grupo 13", col = "red")
boxplot(Serie_B,main = "Box Plot Serie B Grupo 13", col = "green")

#Para la FAs, FAC y FACP
par(mfrow = c(3,2))
acf(Serie_A, main = "FAC Serie A", col = "red")
acf(Serie_B, main = "FAC Serie B", col = "green")
pacf(Serie_A, main = "FACP Serie A", col = "red") 
pacf(Serie_B, main = "FACP Serie B", col = "green") #Otra vez, en primera instancia la serie B no es estacionaria

Serie_B_Diff<-diff(Serie_B) #Diferenciamos la serie

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

#Modelizamos
M1_SerieA<-arima(Serie_A,order = c(1,0,1))
M1_SerieA

M1_SerieB<-arima(Serie_B_Diff,c(1,1,1))
M1_SerieB #Hay raiz unitaria, por lo tanto no es estacionario

