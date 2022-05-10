library(ggplot2)
library(GGally)

data("mtcars")

autos<-mtcars[,c(1:3,6,8)]
# observo como se comportan mis variables
autos$vs<-as.factor(autos$vs)
ggpairs(autos)

#help(ggpairs)

#a) Sólo considero X2
#x: wt, y: mpg
plot(autos$wt,autos$mpg,pch=20, ylim=c(0,40))



g2<-ggplot(autos, aes(x=wt, y=mpg)) + 
  geom_point()+
  xlab("Peso del motor")+
  ylab("Rendimiento")+
  theme_light()
g2

#Buscamos calcular el intervalo de confianza para la respuesta.

#realizamos el ajuste


reg<-lm(mpg~wt,data=autos)
summary(reg)

reg$coefficients
#ic para los coeficientes de la regresion
LI<-summary(reg)$coef[2,1]-qt(0.975,32-2)*summary(reg)$coef[2,2]
LS<-summary(reg)$coef[2,1]+qt(0.975,32-2)*summary(reg)$coef[2,2]

c(LI,LS)


#Una forma de graficarlo sin hacer las cuentas, con ggplot2
g2+ geom_smooth(method="lm", col="firebrick",se=FALSE)+
  labs(title="Consumo Vs peso del motor", 
       y="Consumo", x="Peso del motor")


# Calculamos los intervalos usando la fórmula

# a) Intervalos de CONFIANZA
reg<-lm(mpg~wt,data=autos)
n<-nrow(autos)
X<-model.matrix(reg)
p<-ncol(X) #cant de parametros a estimar

res<-reg$residuals # los residuos son y - y^

s2<-t(res)%*%res/(n-p)
# s2<-summary(reg)$sigma^2 si no quiero hacer la cuenta

A<-solve(t(X)%*%X)



#Podemos hacer la cuenta a mano... yo^=beta0+beta1*x0

IC<-matrix(0,nrow = n,ncol=2)
for(i in 1:n)
{
  IC[i,]<-c(reg$fitted.values[i]-qt(0.975,n-p)*sqrt(s2[1,1]*t(X[i,])%*%A%*%X[i,]),
        reg$fitted.values[i]+qt(0.975,n-p)*sqrt(s2[1,1]*t(X[i,])%*%A%*%X[i,])) 
}


# O dejamos que lo haga R

int<-predict(reg ,interval = "confidence", level = 0.95)


# b) Intervalos de PREDICCION

ICP<-matrix(0,nrow = n,ncol=2)
for(i in 1:n)
{
  ICP[i,]<-c(reg$fitted.values[i]-qt(0.975,n-p)*sqrt(s2[1,1]*(1+t(X[i,])%*%A%*%X[i,])),
            reg$fitted.values[i]+qt(0.975,n-p)*sqrt(s2[1,1]*(1+t(X[i,])%*%A%*%X[i,]))) 
}


# o con R

intP<-predict(reg,interval = "prediction", level = 0.95)


# graficamos con plot

plot(autos$wt,autos$mpg,pch=20, ylim=c(0,40))
abline(reg, col=col1, lwd=2)
points(autos$wt,IC[,1],pch=20,col=col2)
points(autos$wt,IC[,2],pch=20,col=col2)
points(autos$wt,ICP[,1],pch=20,col=col3)
points(autos$wt,ICP[,2],pch=20,col=col3)

# o con ggplot2

# Creamos un nuevo data frame con toda la información

intervalos<-data.frame(cbind(autos,IC,ICP))

g<-ggplot(intervalos)+
  geom_point(aes(x=wt,y=mpg))+
  geom_line(aes(x=wt,y=X1), col="skyblue")+
  geom_line(aes(x=wt,y=X2), col="skyblue")+
  geom_line(aes(x=wt,y=X1.1), col="chocolate",lty=4)+
  geom_line(aes(x=wt,y=X2.1), col="chocolate",lty=4)+
  geom_smooth(aes(x=wt,y=mpg),method="lm", col="firebrick",se=FALSE)+
  labs(title="Consumo Vs peso del motor - Intervalos", 
       y="Consumo", x="Peso del motor")+
  theme_light()
g



# Bandas de confianza

BC<-matrix(0,nrow = n,ncol=2)
for(i in 1:n)
{
  BC[i,]<-c(reg$fitted.values[i]-sqrt(p*qf(0.95,p,n-p))*sqrt(s2[1,1]*(t(X[i,])%*%A%*%X[i,])),
             reg$fitted.values[i]+sqrt(p*qf(0.95,p,n-p))*sqrt(s2[1,1]*(t(X[i,])%*%A%*%X[i,]))) 
}

intervalos<-data.frame(cbind(autos,IC,ICP,BC))
g<-ggplot(intervalos)+
  geom_point(aes(x=wt,y=mpg))+
  geom_line(aes(x=wt,y=X1), col="skyblue")+
  geom_line(aes(x=wt,y=X2), col="skyblue")+
  geom_line(aes(x=wt,y=X1.1), col="chocolate",lty=4)+
  geom_line(aes(x=wt,y=X2.1), col="chocolate",lty=4)+
  geom_line(aes(x=wt,y=X1.2), col="chartreuse4",lwd=1.5)+
  geom_line(aes(x=wt,y=X2.2), col="chartreuse4",lwd=1.5)+
  geom_smooth(aes(x=wt,y=mpg),method="lm", col="firebrick",se=FALSE)+
  labs(title="Consumo Vs peso del motor - Intervalos", 
       y="Consumo", x="Peso del motor")+
  theme_light()
g

confint(reg)

