#####
ej 2
#3
af<-read.table("af.txt",header = TRUE)
apf<-read.table("apf.txt", header = TRUE)
Yaf<-cbind(af$antenas+af$alas,af$alas)
Yapf<-cbind(apf$Antenas+apf$alas,apf$alas)
yrayaaf<-apply(Yaf,2,mean)
yrayaapf<-apply(Yapf,2,mean)
n1<-nrow(Yaf)
n2<-nrow(Yapf)
saf<-cov(Yaf)
sapf<-cov(Yapf)

q1<-saf*(n1-1)
q2<-sapf*(n2-1)
q<-q1+q2
gamma<-(det(q1/n1)^(n1/2))*(det(q2/n2)^(n2/2))/(det((q/(n1+n2)))^((n1+n2)/2))
-2*log(gamma)
qchisq(0.99,3)

U<-q
yraya<-(n1*yrayaaf+n2*yrayaapf)/(n1+n2)
H<-n1*(yrayaaf-yraya)%*%t(yrayaaf-yraya)+n2*(yrayaapf-yraya)%*%t(yrayaapf-yraya)
T<-chol(U)
B<-t(solve(T))%*%H%*%solve(T)
b<-eigen(B)
a1<-(solve(T))%*%b$vectors[,1]*sqrt((n1+n2)-2)#CON O SIN t?
a2<-(solve(T))%*%b$vectors[,2]*sqrt((n1+n2)-2)

A<-cbind(a1,a2)

z1<-t(A)%*%t(Yaf)

z2<-t(A)%*%t(Yapf)


z1af<-t(A)%*%yrayaaf
z1apf<-t(A)%*%yrayaapf


plot(z1[1,],z1[2,],ylim=c(-18,-15), xlim=c(-6,2),col="blue")
points(z2[1,],z2[2,],col="red")
points(z1af[1],z1af[2],col="green")
points(z1apf[1],z1apf[2])


#como dibujas solo la primera coordenada discriminante?
z1<-t(a1)%*%t(Yaf)
z2<-t(a1)%*%t(Yapf)
plot(z1,rep(2,9),ylim=c(1,3), xlim=c(-6,2),col="blue")
points(z2,rep(2,6),col="red")
points(z1af[1],2,col="green")
points(z1apf[1],2)


#la segunda
z1<-t(a2)%*%t(Yaf)
z2<-t(a2)%*%t(Yapf)
plot(z1,rep(2,9),ylim=c(1,3), xlim=c(-18,-15),col="blue")
points(z2,rep(2,6),col="red")
points(z1af[2],2,col="green")
points(z1apf[2],2)

#c
s<-((n1-1)*saf+(n2-1)*sapf)/(n1+n2-2)
apico<-solve(s)%*%(yrayaaf-yrayaapf)
m<-t(apico)%*%(yrayaaf+yrayaapf)/(2*(apico[2,1]))
plot(Yaf[,1],Yaf[,2],col="blue")
points(Yapf[,1],Yapf[,2],col="red")
points(yrayaaf[1],yrayaaf[2],col="green")
points(yrayaapf[1],yrayaapf[2])
abline(m,-apico[1,1]/apico[2,1])
