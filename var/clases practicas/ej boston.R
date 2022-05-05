library(mlbench)
library(GGally)
library(ggplot2)

data("BostonHousing")
#help(BostonHousing)

x1<-BostonHousing$nox
x2<-BostonHousing$rm
x3<-BostonHousing$lstat
y<-BostonHousing$medv

# o mejor

datos<-BostonHousing[,c(5,6,13,14)] #me quedo solo con las variables que me interesan
pairs(datos, pch=20)
cor(datos)

ggpairs(datos)
ggcorr(datos)

#modelo propuesto
# y= beta0+beta1 nox+beta2 rm + beta3 lstat+ eps
reg<-lm(y~x1+x2+x3)
reg<-lm(medv~nox+lstat+rm, data=datos)
reg<-lm(medv~., data=datos) #las tres dan lo mismo


summary(reg)

#############
s<-summary(reg)$sigma

