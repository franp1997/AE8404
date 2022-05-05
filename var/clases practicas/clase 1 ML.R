library(ggplot2)

#cargo los datos
datos<-cars

#defino las variables
x<-datos$speed #velocidad del auto
y<-datos$dist #distancia requerida de frenado

#grafico bonitos
ggplot(data=datos)+ 
  geom_point(aes(x=speed, y=dist))+
  labs(title="",
       x="Velocidad", 
       y = "Distancia de frenado")+
  theme_light()

#grafico mas facil
plot(x,y,pch=20, col="deepskyblue")


x_raya<-mean(x) #promedio de los elementos de x
s_x<-sd(x) #devio estandar muestral

#estimar beta0 y beta1


X<-cbind(rep(1,length(x)),x)


t(X) #transpuesta de X
X%*%X #producto entre matrices
solve(X) # te da la inversa 

beta_sombrero<-solve(t(X)%*%X)%*%t(X)%*%y

beta_sombrero[1] #elemento 1 del vector

#modelo ajustado
# y_sombrero = -17.579 + 3.93* x

plot(x,y,pch=20,col="deepskyblue")
#grafico mi recta predicha
y_sombrero<-beta_sombrero[1]+beta_sombrero[2]*x
lines(x,y_sombrero,col="chocolate" )

points(x[10],y[10],col="red")

#otra forma de hacer la recta
abline(beta_sombrero[1],beta_sombrero[2],col="Forestgreen")

#o directamente
abline(beta_sombrero)

#lo que pide el ejercicio
predichos<--17.579 + 3.93* x
points(x,predichos,col="chocolate",pch=20)


#con ggplot2
datos2<-data.frame(cbind(datos,y_sombrero))
ggplot(data=datos2)+ 
  geom_point(aes(x=speed, y=dist))+
  geom_line(aes(x=speed,y=y_sombrero),col="Firebrick")+
  labs(title="",
       x="Velocidad", 
       y = "Distancia de frenado")+
  theme_light()

#estimamos la varianza
S2<-t(y-y_sombrero)%*%(y-y_sombrero)/(50-2)

# de otra forma
s2<-sum((y-predichos)^2)/(length(x)-2)

sqrt(S2)


# ahora la versión corta

ajuste<-lm(y~x)
ajuste
summary(ajuste)

# que hay en ajuste?
names(ajuste)

# que hay en el resumen de ajuste?
names(summary(ajuste))

# como saco la estimacion del sigma de la salida
summary(ajuste)$sigma

#como puedo sacar cada beta sombrero 
beta_som_0<-ajuste$coefficients[1]

