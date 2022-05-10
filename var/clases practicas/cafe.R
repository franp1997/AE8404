library(readxl)
library(ggplot2)
cafe<-read_excel("Cafeina.xls")
n<-nrow(cafe)
cafe<-read.table("cafeina.txt",header=TRUE)


# Test F haciendo las cuentas
X<-cbind(c(rep(1,10),rep(0,20)),c(rep(0,10),rep(1,10),rep(0,10)),c(rep(0,20),rep(1,10)))
p<-ncol(X)
C<-rbind(c(1,-1,0),c(0,1,-1))
mu_s<-c(mean(cafe$y[1:10]),mean(cafe$y[11:20]),mean(cafe$y[21:30]))
s_2<-t(cafe$y-X%*%mu_s)%*%(cafe$y-X%*%mu_s)/(n-p)
q<-nrow(C)
f_obs<-t(C%*%mu_s)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%mu_s)/q/s_2 # es el f observado
qf(0.95,q,n-p) #este es el cuantil, limite de mi zona de rechazo
1-pf(f_obs,q,n-p) #este es el p-valor


# Ahora buscamos los intervalos

#Bonferroni

c1<-c(1,-1,0) #compara no cafeina con 100
c2<-c(1,0,-1) # compara no cafeina con 200
c3<-c(0,1,-1) # compara 100 con 200
qb<-3
alfa<-0.05 #mi nivel global

ic1<-c(t(c1)%*%mu_s-qt(1-alfa/(2*qb),n-p)*sqrt(s_2*t(c1)%*%solve(t(X)%*%X)%*%c1),
       t(c1)%*%mu_s+qt(1-alfa/(2*qb),n-p)*sqrt(s_2*t(c1)%*%solve(t(X)%*%X)%*%c1))

ic2<-c(t(c2)%*%mu_s-qt(1-alfa/(2*qb),n-p)*sqrt(s_2*t(c2)%*%solve(t(X)%*%X)%*%c2),
       t(c2)%*%mu_s+qt(1-alfa/(2*qb),n-p)*sqrt(s_2*t(c2)%*%solve(t(X)%*%X)%*%c2))

ic3<-c(t(c3)%*%mu_s-qt(1-alfa/(2*qb),n-p)*sqrt(s_2*t(c3)%*%solve(t(X)%*%X)%*%c3),
       t(c3)%*%mu_s+qt(1-alfa/(2*qb),n-p)*sqrt(s_2*t(c3)%*%solve(t(X)%*%X)%*%c3))

ic1
ic2
ic3



# Sheffe

h1<-c(1,0)
h2<-c(0,1)
h3<-c(1,1)

IC1<-c(t(h1)%*%C%*%mu_s-sqrt(q*qf(1-alfa,q,n-p))*sqrt(s_2*t(h1)%*%C%*%solve(t(X)%*%X)%*%t(C)%*%h1),
     t(h1)%*%C%*%mu_s+sqrt(q*qf(1-alfa,q,n-p))*sqrt(s_2*t(h1)%*%C%*%solve(t(X)%*%X)%*%t(C)%*%h1))

IC2<-c(t(h2)%*%C%*%mu_s-sqrt(q*qf(1-alfa,q,n-p))*sqrt(s_2*t(h2)%*%C%*%solve(t(X)%*%X)%*%t(C)%*%h2),
       t(h2)%*%C%*%mu_s+sqrt(q*qf(1-alfa,q,n-p))*sqrt(s_2*t(h2)%*%C%*%solve(t(X)%*%X)%*%t(C)%*%h2))
IC3<-c(t(h3)%*%C%*%mu_s-sqrt(q*qf(1-alfa,q,n-p))*sqrt(s_2*t(h3)%*%C%*%solve(t(X)%*%X)%*%t(C)%*%h3),
       t(h3)%*%C%*%mu_s+sqrt(q*qf(1-alfa,q,n-p))*sqrt(s_2*t(h3)%*%C%*%solve(t(X)%*%X)%*%t(C)%*%h3))

IC1
IC2
IC3

cafe$dosis<-as.factor(cafe$dosis)
boxplot(cafe$y~cafe$dosis)
ggplot(cafe, aes(x= dosis, y=y,fill=dosis))+
  geom_boxplot() + 
  labs(title="Box plot",
       caption="Source: ",
       x="Dosis",
       y="Cantidad")+theme_light()

#con tukey
reg<-aov(y~dosis,data=cafe) #en este caso necesita la tabla de anova
TukeyHSD(reg)
plot(TukeyHSD(reg))
