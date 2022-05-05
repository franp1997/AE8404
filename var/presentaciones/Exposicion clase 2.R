library(ggplot2)
# 1) Lectura de los datos.
datos<-read.csv("islander_data.csv")

# 2) Realizar un histograma para la variable X :diferencia de tiempos
X<-datos$Diff
xmin<-min(X)
xmax<-max(X)
print(xmin)
print(xmax)

g1<-ggplot(datos, aes(X1))+
  geom_histogram(aes(x=X,y=..density..),binwidth = 10,alpha=5,
                 fill="royalblue1",color="black",
                 breaks=seq(xmin,xmax,1))
theme_light()

print(g1)

# 3) Graficar la funcion empirica para X.
plot(ecdf(X))


# 4) Estimar la densidad de X usando estimadores basados en nucleos, utilizando 
#diferentes tamaños de ventana. ¿Que observa?

# Estimador de Parzen
uniforme <-function(u)
{
  ifelse(u>-1 & u<1,1,0)/2  
}

#para nucleos
f_sombrero<-function(x,k,datos,h) #datos =Xi
{
  s<-0 #inicializo variable s
  i<-0
  for(i in 1:length(datos))
  {
    c<-k((x-datos[i])/h)
    s<-s+c
  }
  f<-s/(length(datos)*h)
  return(f)
}

densidad.est.parzen<- function(x,h,z) #x:datos, z:valor, donde se evalua la f
{
  f_sombrero(z,uniforme,x,h)
}



nuevos<-seq(xmin,xmax, length=198)
h<-3.12
f_estimada1<-densidad.est.parzen(X,h,nuevos)
f_est<-data.frame("x"=nuevos, "estimada1"=f_estimada1)
f_est$estimada1<f_est
ggplot(f_est,aes(x=x,y=estimada1))+
  geom_line(col="black")+
  theme_classic()
max(f_est$estimada1)


###############################

#Nuclos por defecto de R.
#help density
#density(x, bw = "nrd0", adjust = 1,
#kernel = c("gaussian", "epanechnikov", "rectangular",
#          "triangular", "biweight",
#         "cosine", "optcosine")


hist(datos$Diff,breaks=100,col ="lightblue",freq=FALSE,ylim=c(0,0.08))

lines(density(datos$Diff,kernel = "gaussian",window=h),col="red", lwd=3)
lines(density(datos$Diff,kernel = "epanechnikov",window=h),col="black", lwd=2)
lines(density(datos$Diff,kernel = "triangular",window=h),col="royalblue", lwd=1.5)

#seleccion de ventanas

#Ventana de silverman

h_sil<- ((4*sd(datos$Diff)^5/(3*length(datos$Diff)))^(1/5))
print(h_sil)

# NOTA no hace falta redondear las ventanas.

#Saquemos las ventanas por cv (Validacion cruzada ó cross validation)

h<-seq(h_sil*0.5,h_sil*2,(h_sil/100))
plvH<-c()
for(j in 1:length(h))
{
  plv<-c()
  for(i in 1:length(datos$nieve))
  {
    f_hat.x<-f_sombrero(datos$Diff[i],epa,datos$Diff[-i],h[j])
    plv[i]<-log(f_hat.x)
  }
  plvH[j]<-mean(na.omit(plv))
}

plot(h,plvH,type="l")

#plot(h[10:30],plvH[10:30],type="l")


h[which.max(plvH)]



