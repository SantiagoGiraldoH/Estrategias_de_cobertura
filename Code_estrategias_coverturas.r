#Leer los Datos
datos = read.csv("TRMlimpio.csv", sep = ";", dec = ",", header = F)
#Precios
precios <- datos [,2]
precios <- ts(precios)
plot(precios, main="precio")

#Rendimientos
rendimientos <- diff(log(precios))

s = tail(precios,1)
s = as.numeric(s)

mu = mean(rendimientos) #diario
volatilidad = sd(rendimientos)

# estrategias de coberturas con opciones 

k1 <- s - 50
k2 <- s
        
r <- 0.0171/(30/360) 
r <- log(1+r)/30

rf <- 0.0015013/(30/360)
rf <- log(1+rf)/30

T <- 30 
dt <- 1
iteraciones <- 50000

set.seed(1)

#ST prima en mundo neutral al riesgo
st_prima = matrix(, iteraciones, T+1)

st_prima[,1] = s

for(i in 1:iteraciones){
        
        for(j in 2:(T+1)){
                
                st_prima[i,j] = st_prima[i,j-1]*exp((r/360-rf/360-(volatilidad^2)/2)*dt+volatilidad*sqrt(dt)*rnorm(1))
                
        }
}

compensacionesCall<- vector()

compensacionesPut1 <- vector()
compensacionesPut2 <- vector()

for(i in 1:iteraciones){
        compensacionesCall[i] <- max(st_prima[i,T+1]-k2,0)*exp(-(r-rf)*1/12)
        compensacionesPut1[i] <- max(k2-st_prima[i,T+1],0)*exp(-(r-rf)*1/12)
        compensacionesPut2[i] <- max(k1-st_prima[i,T+1],0)*exp(-(r-rf)*1/12)
}

primacall <- mean(compensacionesCall)

primaput1 <- mean(compensacionesPut1)
primaput2 <- mean(compensacionesPut2)

#St con riesgo 

st <- matrix(, iteraciones, T+1)
st[,1] <- s
for(i in 1:iteraciones){
        for(j in 2:(T+1)){
                st[i,j] <- st[i,j-1]*exp((mu-volatilidad^2/2)*dt+volatilidad*sqrt(dt)*rnorm(1))
        }
}

#precios con coberturas
coberturaCall <- vector()
coberturaFwd <- vector()
coberturaCollar <- vector()

for(i in 1:iteraciones){
        coberturaCall[i] <- abs(-st[i,T+1] + max(st[i,T+1]-k2,0)-primacall)
        coberturaFwd[i] <- abs(-st[i,T+1] + max(st-k2,0)-primacall + min(st[i,T+1]-k2,0)+primaput1)
        coberturaCollar[i] <- abs(-st[i,T+1] + max(st[i,T+1] - k2,0)-primacall + min(st[i,T+1]-k1,0)+primaput2)
        
}

#resutlados 
resultados <- data.frame(st[,T+1], coberturaCall, coberturaFwd, coberturaCollar)

colnames(resultados) <- c("Sin coberturas", "coberturaCall", "coberturaFwd", "coberturaCollar")

library(ggplot2)
#Sin cobertura
mean(resultados[,"Sin coberturas"])

quantile(resultados[,"Sin coberturas"], c(0.05, 0.95))

sd(resultados[,"Sin coberturas"])

#Cobertura Call

mean(resultados[,"coberturaCall"])

quantile(resultados[,"coberturaCall"], c(0.05, 0.95))

sd(resultados[,"coberturaCall"])

#Cobertura Fwd
mean(resultados[,"coberturaFwd"])

quantile(resultados[,"coberturaFwd"], c(0.05, 0.95))

sd(resultados[,"coberturaFwd"])

#Cobertura Collar

mean(resultados[,"coberturaCollar"])

quantile(resultados[,"coberturaCollar"], c(0.05, 0.95))

sd(resultados[,"coberturaCollar"])

#Grafico sin cobertura
ggplot(data = resultados, aes(resultados[,"Sin coberturas"]))+
        geom_histogram(aes(y=..density..),binwidth = 50, colour = "darkgray", fill = "darkgray")+
        geom_vline(xintercept = quantile(resultados[,"Sin coberturas"], c(0.05, 0.95)), colour="black", size=1.5)+
        labs(title = "Escenario sin cobertura", x = "Precio")

#comparacion sin cobertura y cobertura call
ggplot(data = resultados, aes(resultados[,"Sin coberturas"]))+
        geom_histogram(aes(y=..density..),binwidth = 50, colour = "darkgray", fill = "darkgray")+
        geom_vline(xintercept = quantile(resultados[,"Sin coberturas"], c(0.05, 0.95)), colour="black", size=1.5)+
        geom_vline(xintercept = quantile(resultados[,"coberturaCall"],c(0.05, 0.95)), colour="darkgreen", size=1.5)+
        labs(x = "Precio")

#comparacion sin cobertura y cobertura Forward participativo
ggplot(data = resultados, aes(resultados[,"Sin coberturas"]))+
        geom_histogram(aes(y=..density..),binwidth = 50, colour = "darkgray", fill = "darkgray")+
        geom_vline(xintercept = quantile(resultados[,"Sin coberturas"], c(0.05, 0.95)), colour="black", size=1.5)+
        geom_vline(xintercept = quantile(resultados[,"coberturaFwd"],c(0.05, 0.95)), colour="darkgreen", size=1.5)+
        labs(x = "Precio")

#comparacion sin cobertura y cobertura Collar
ggplot(data = resultados, aes(resultados[,"Sin coberturas"]))+
        geom_histogram(aes(y=..density..),binwidth = 50, colour = "darkgray", fill = "darkgray")+
        geom_vline(xintercept = quantile(resultados[,"Sin coberturas"], c(0.05, 0.95)), colour="black", size=1.5)+
        geom_vline(xintercept = quantile(resultados[,"coberturaCollar"],c(0.05, 0.95)), colour="darkgreen", size=1.5)+
        labs(x = "Precio")

#Comparación 3 estrategias
ggplot(data = resultados, aes(resultados[,"Sin coberturas"]))+
        geom_histogram(aes(y=..density..),binwidth = 50, colour = "darkgray", fill = "darkgray")+
        geom_vline(xintercept = quantile(resultados[,"Sin coberturas"], c(0.05, 0.95)), colour="black", size=1.5)+
        geom_vline(xintercept = quantile(resultados[,"coberturaCall"],c(0.05, 0.95)), colour="darkgreen", size=1.5)+
        geom_vline(xintercept = quantile(resultados[,"coberturaCollar"],c(0.05, 0.95)), colour="darkblue", size=1.5)+
        geom_vline(xintercept = quantile(resultados[,"coberturaFwd"],c(0.05, 0.95)), colour="darkred", size=1.5)+
        labs(x = "Precio")

ggplot(resultados, aes(resultados[,"Sin coberturas"])) + geom_histogram(binwidth = 50, alpha = 0.5, colour = "darkgray", fill = "darkgray")+
        geom_histogram(aes(resultados[,"coberturaCall"]), alpha = 0.3, binwidth = 50, colour = "darkgreen", fill = "darkgreen")+
        geom_histogram(aes(resultados[,"coberturaCollar"]), alpha = 0.3, binwidth = 50, colour = "darkblue", fill = "darkblue")+
        geom_histogram(aes(resultados[,"coberturaFwd"]), alpha = 0.3, binwidth = 50, colour = "darkred", fill = "darkred")
        labs(title = "Histogramas", x = "Precio", y = "Frecuencia")
        