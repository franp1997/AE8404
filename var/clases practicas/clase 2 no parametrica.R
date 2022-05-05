library(ggplot2)
library(gridExtra)


####### REGRESION NP------

#Simulado discreto, veamos el ejemplo
#
# X: cantidad de bolitas verdes en 3 extracciones sin reposición
# Y: cantidad de bolitas rojas en 3 extracciones sin reposición

urna<-c(rep("R",3),rep("V",4),rep("A",3))
n<-10000
set.seed(27)
xy<-matrix(0,nrow = n,ncol = 2)
for(i in 1:n)
{
  muestra<-sample(urna,3)
  xy[i,]<-c(sum(muestra=="V"),sum(muestra=="R"))
}

plot(xy[,1],xy[,2],pch=20, col="darkblue", xlab="cantidad de verdes", ylab="cantidad de rojas")


M<-table(xy[,1],xy[,2])/n
M

#grafico del soporte en el que puedo ver los pesos (interesante...)
datos<-as.data.frame(M)
pp<-ggplot(datos, aes(Var1,Var2))+
  geom_point(aes(size=Freq))+
  labs(title="Función de probabilidad conjunta",
       y = "y", x= "x")
pp

# ¿como calculo lo que quiero? vectores logicos...
y<-xy[,2]
x<-xy[,1]
fi_sombrero0<-sum(y[x==0])/sum(x==0)
abline(h=mean(y), col="darkolivegreen")


y0<-mean(xy[xy[,1]==0,2])
y1<-mean(xy[xy[,1]==1,2])
y2<-mean(xy[xy[,1]==2,2])
y3<-mean(xy[xy[,1]==3,2])
ey<-c(y0,y1,y2,y3)


plot(xy[,1],xy[,2],pch=20, col="darkblue",xlab="x",ylab="y")
points(0:3,ey,pch=20,col="chocolate")
lines(0:3,ey,pch=20,col="chocolate",lwd=1.5)

##############################################

# Ejercicio para desarrollar en clase

# eLIDAR (light detection and ranging)
# range --> x
# logratio --> y

help("ksmooth")
help("density")

datos<-read.table("lidar.txt",header = TRUE)

#1
plot(datos$range,datos$int.conc,pch=20,col="skyblue",
     xlab= "Ratio", ylab = "Logaritmo del cociente")

# Nadaraya watson usando R, si no te armas la funcion

regNP<-ksmooth(datos$range,datos$int.conc, kernel = "normal", bandwidth = 5,
               x.points = datos$range)
y<-datos$int.conc
ecpp<-sum((y-regNP)^2)/length(y)


lines(regNP$x,regNP$y,col= "chocolate",lwd=2)

regNP2<-ksmooth(datos$range,datos$int.conc,kernel = "normal", bandwidth = 10,
                x.points = datos$range)
lines(regNP2$x,regNP2$y,col= "darkred",lwd=2)

regNP3<-ksmooth(datos$range,datos$int.conc,kernel = "normal", bandwidth = 30,
                x.points = datos$range)
lines(regNP3$x,regNP3$y,col= "forestgreen",lwd=2)

regNP4<-ksmooth(datos$range,datos$int.conc,kernel = "normal", bandwidth = 50,
                x.points = datos$range)
lines(regNP4$x,regNP4$y,col= "red",lwd=2)


# ¿Que pasa cuando aumento h?


# Errores sobre los datos que use para predecir

ecpp1<-mean((datos$int.conc-regNP$y)^2)
ecpp2<-mean((datos$int.conc-regNP2$y)^2)
ecpp3<-mean((datos$int.conc-regNP3$y)^2)
ecpp4<-mean((datos$int.conc-regNP4$y)^2)

c(ecpp1,ecpp2,ecpp3,ecpp4) #menor h, menor eccp

# LOO-CV Busco la ventana optima

h<-seq(3,165,1)

ecm<-c()
cv<-c()
for(j in 1:length(h))
{
  for(i in 1:length(datos$range))
  {
    regNP<-ksmooth(datos$range[-i],datos$int.conc[-i],kernel = "normal", bandwidth = h[j],
                   x.points = datos$range[i])
    ecm[i]<-(datos$int.conc[i]-regNP$y)^2
  }
  cv[j]<-mean(ecm)
}

plot(h,cv,pch=20) 

h_CV<-h[which.min(cv)]

# Vemos como queda la mejor

plot(datos$range,datos$int.conc,pch=20,col="skyblue",
     xlab= "Ratio", ylab = "Logaritmo del cociente")

regNP<-ksmooth(datos$range,datos$int.conc,kernel = "normal", bandwidth = h_CV,
               x.points = datos$range)
lines(regNP$x,regNP$y,col= "chocolate",lwd=2)

# error con el mejor

ecpp5<-mean((datos$int.conc-regNP$y)^2)


# Sacamos una mejor estimacion del error con el mejor (usando CV)

ecm<-c()
for(i in 1:length(datos$range))
{
  regNP<-ksmooth(datos$range[-i],datos$int.conc[-i],kernel = "normal", bandwidth = h_CV,
                 x.points = datos$range[i])
  ecm[i]<-(datos$int.conc[i]-regNP$y)^2
}
ecv<-mean(ecm)

