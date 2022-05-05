library(ggplot2)
library(gridExtra)

# Primeros ejemplos para simular y aprender un poco de R:

# 1) Tiramos un dado y estimamos la probabilidad de que salga el 1
dado<-c(1,2,3,4,5,6)
x<-sample(dado, 10000, replace=TRUE)
sum(x==1)/10000
1/6
# 2) Sacamos 2 bolitas de una urna que contiene 5 verdes y 3 rojas, 
#   y estimamos  la probabilidad de que ambas sean del mismo color. 
#   Estimamos también la probabilidad de que la segunda bolita sea roja 
#   sabiendo que la primera fue verde.


# Variables

## Discretas

astro<-read.csv("astronauts.csv")
X<-astro$Space.Flights

# estimamos la función de probabilidad
barplot(table(X)/length(X))
plot(ecdf(X))

# mas lindo (fuera de programa)
#grafico de barras

datos<-data.frame(table(X)/length(X))
ggplot(datos, aes(x=as.numeric(X),y=Freq))+
  geom_bar(stat="identity", width=0.02)+
  theme_light()

# Continuas

# Histograma

library()
bufalo<-scan("buffalo.txt")
datos<-data.frame("nieve"= bufalo)

# Histograma moviendo x0
g1<-ggplot(datos, aes(X1)) + 
  geom_histogram(aes(x=nieve,y = ..density..),binwidth = 2,alpha=0.5, 
                 fill = "mediumpurple3", color = "black",
                 breaks =seq(20,130,10)) + 
  theme_light()

g2<-ggplot(datos, aes(X1)) + 
  geom_histogram(aes(x=nieve,y = ..density..),binwidth = 2,alpha=0.5, 
                 fill = "mediumpurple3", color = "black",
                 breaks =seq(22,132,10)) + 
  theme_light()

g3<-ggplot(datos, aes(X1)) + 
  geom_histogram(aes(x=nieve,y = ..density..),binwidth = 2,alpha=0.5, 
                 fill = "mediumpurple3", color = "black",
                 breaks =seq(24,134,10)) + 
  theme_light()

g4<-ggplot(datos, aes(X1)) + 
  geom_histogram(aes(x=nieve,y = ..density..),binwidth = 2,alpha=0.5, 
                 fill = "mediumpurple3", color = "black",
                 breaks =seq(26,136,10)) + 
  theme_light()
grid.arrange(g1, g2, g3,g4, nrow = 2)


# Estimadores basados en nucleos

# Estimacion Parzen
uniforme<-function(u)
{
  ifelse(u>-1 & u<1,1,0)/2
}


#para nucleos
f_sombrero<-function(x,k,datos,h) #datos= Xi
{
  s<-0
  for(i in 1:length(datos))
  {
    c<-k((x-datos[i])/h)
    s<-s+c
  }
  f<-s/(length(datos)*h)
  return(f)
}  


densidad.est.parzen<-function(x,h,z) # x: datos, z:valor donde exaluo la f
{
  f_sombrero(z,uniforme,x,h)
}


nuevos<-seq(25,126.4,length=200)
h<-10
f_estimada1<-densidad.est.parzen(datos$nieve,h,nuevos)
f_est<-data.frame("x"=nuevos,  "estimada1"=f_estimada1)

ggplot(f_est,aes(x=x,y=estimada1))+
  geom_line(col="steelblue")+
  theme_light()


h<-20
f_est$estimada2<-densidad.est.parzen(datos$nieve,h,nuevos)

h<-30
f_est$estimada3<-densidad.est.parzen(datos$nieve,h,nuevos)


ggplot(f_est)+
  geom_histogram(data=datos,aes(x=nieve,y = ..density..),binwidth = 2,alpha=0.3, 
                 fill = "mediumpurple3", color = "black",
                 breaks =seq(25,126.4,10)) + 
  geom_line(aes(x=x,y=estimada1),col="steelblue",lwd=1.5)+
  geom_line(aes(x=x,y=estimada2),col="firebrick",lwd=1.5)+
  geom_line(aes(x=x,y=estimada3),col="olivedrab4",lwd=1.5)+
  theme_light()



#Con density. Density evalua sobre una grilla equiespaciada
h<-5

#help(density)  kernel = c("gaussian", "epanechnikov", "rectangular",
 #                         "triangular", "biweight",
  #                        "cosine", "optcosine"),

hist(datos$nieve, freq = FALSE,ylim=c(0,0.02))

lines(density(datos$nieve,kernel = "gaussian",window=h),col="yellowgreen",lwd=2)
lines(density(datos$nieve,kernel = "epanechnikov",window=h),col="firebrick",lwd=2)
lines(density(datos$nieve,kernel = "rectangular",window=h),col="steelblue",lwd=2)



# Uso mi función que da lo mismo que el density, pero la puedo evaluar en los puntos que yo quiero.
gauss<-function(u)
{
  k<-exp(-(u^2)/2)/sqrt(2*pi)
  return(k)
}

estimada5<-f_sombrero(sort(datos$nieve),gauss,datos$nieve,10)


# Otros nucleos
epa<-function(u)
{
  ifelse(abs(u) < 1,3/4*(1-u^2),0)
} 


# selección de la ventana

#ventana de silverman...

h_sil<-round((4*sd(datos$nieve)^5/(3*length(datos$nieve)))^(1/5),0)


# saquemos la ventana por cv
h<-seq(5,40,1)
plvH<-c()
for(j in 1:length(h))
{
  plv<-c()
  for(i in 1:length(datos$nieve))
  {
    f.hat.x<-f_sombrero(datos$nieve[i],epa,datos$nieve[-i],h[j])
    plv[i]<-log(f.hat.x)
  }
  plvH[j]<-mean(na.omit(plv))
}

plot(h,plvH,type="l") #pasa que para muchas ventanas no encontró datos...no las tengo en cuenta

plot(h[10:30],plvH[10:30],type = "l")

h[which.max(plvH)]

