#ejemplo de boot paramétrico

# tiempos de vida de 30 lamparas
x<-c(3.5, 4.5, 3.1, 4.2, 2.4, 2.8, 3.0, 3.8, 2.5, 2.7,
     2.8, 2.6, 3.4, 2.5, 3.6, 2.6, 3.1, 4.0, 2.3, 3.5,
     4.1, 2.9, 2.6, 2.0, 3.0, 2.5, 2.9, 3.1, 2.3, 2.8)
boxplot(x)
hist(x,freq=FALSE)

library(MASS)
# me gustaría usar el supuesto de que estos datos provienen de una población con distribución Gamma
est <- fitdistr(x, densfun = "gamma")$estimate #busco los estimadores de los parametros

Nboot<-1000
mediana<-c()
for(i in 1:Nboot)
{
  nueva_muestra<-rgamma(length(x),shape=est[1],rate = est[2])
  mediana[i]<-median(nueva_muestra)
}

hist(mediana)
sd(mediana)


# si fuera no parametrico
nueva_muestra_np<-sample(x,length(x),replace = TRUE)


#Bootstrap - Ejercicio de clase
set.seed(27)
n <- 500
x<- rexp(n)

hist(x, freq= FALSE)
boxplot(x)
mediana<-median(x)
mediana
promedio<-mean(x)
promedio

Nboot<-1000
medianas<- c()
medias<-c()
for(i in 1:Nboot) 
  {
    indice<-sample(1:n, replace = TRUE)
    medianas[i]<-median(x[indice]) 
    medias[i]<-mean(x[indice])
  }


par(mfrow=c(1,2))
hist(medianas, xlim=c(0.5, 1.2), freq=FALSE)
hist(medias,xlim=c(0.5, 1.2), freq=FALSE)
par(mfrow=c(1,1))


var_estimada_boot<-sd(medianas)^2
alfa <- 0.05

#ic asintotico suponiendo normalidad

LI_mediana<- mediana-qnorm(1-alfa/2)*sqrt(var_estimada_boot)
LS_mediana<- mediana+qnorm(1-alfa/2)*sqrt(var_estimada_boot)
c(LI_mediana,LS_mediana)


# Bootstrap percentil

IC.median.per_LI<- quantile(medianas, probs = alfa/2, na.rm = T)
IC.median.per_LS<- quantile(medianas, probs = 1-alfa/2, na.rm = T)
c(IC.median.per_LI,IC.median.per_LS)



# xtra, jugando para probar la normalidad
shapiro.test(medianas) #rechaza hipótesis de normalidad
shapiro.test(medias)  #no rechaza hipótesis de normalidad

#mediana
qqnorm(medianas)
qqline(medianas) #colas pesadas

#media
qqnorm(medias)
qqline(medias) #divino

