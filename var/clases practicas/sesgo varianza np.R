color<-c("firebrick",
         "dodgerblue3",
         "goldenrod1",
         "darkolivegreen4",
         "darkorange1",
         "chocolate",
         "darkblue",
         "forestgreen",
         rgb(0.2,0.4,0.6,0.6),
         "mediumpurple3")

# inventamos un set de datos
n<-1000
x<-seq(0.1,100,length=n)
eps<-rnorm(n)
y<-log(10*x+1)+eps
plot(x,y,pch=20, col="dodgerblue3")
 
lines(x,log(10*x+1),col="chocolate", lwd=4)

# simulamos varias muestras y graficamos la regresión no para metrica
# la complejidad del modelo en este caso estaría dada por h
# a mayor h menos complejo, a menor h más complejo,
# ¿cumple con lo visto en la teoria? juguemos

#movemos h desde 1 hasta 50
h<-1
plot(x,log(10*x+1),col="chocolate",type="l", lwd=4)
for(i in 1:10)
{
  eps<-rnorm(n)
  y<-log(10*x+1)+eps
  reg<-ksmooth(x,y,kernel = "normal", bandwidth = h,
               x.points = x)
  lines(reg$x,reg$y,col=color[i], lwd=3)
}



