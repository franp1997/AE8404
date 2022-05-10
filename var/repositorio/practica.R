library(data.table)
library(ggplot2)
DT <- fread("data/lidar.txt", sep=" ") |> setnames(old="int.conc",new="logratio")

ggplot(DT)+geom_point(aes(x=range,y=logratio),pch=20,color="tomato3")  +theme_bw()


NW <- NULL
for(h in c(1,5,10,15,30,50)){
  K <- ksmooth(x=DT$range,y=DT$logratio,bandwidth = h)
  MODEL <- lm(data=K, formula = y ~ x)
  NW <- rbindlist(list(NW,data.table(h=h,rangeKS=K$x,logratioKS=K$y,logratioLM=predict(MODEL))))
}

ggplot(NW[,h:=as.factor(h)])+ geom_line(aes(x=rangeKS,y=logratioKS,color=h)) +theme_bw()

ggplot(NW[,h:=as.factor(h)])+ geom_line(aes(x=rangeKS,y=logratioLM),color=h)  +theme_bw()


ggplot(NW[,h:=as.factor(h)])+ geom_line(aes(x=rangeKS,y=logratioKS,color=h))+ geom_line(aes(x=rangeKS,y=logratioLM)) + facet_wrap(~h) +theme_bw() 


# DTS <- ksmooth(x=DT$x,y=DT$y,bandwidth = 10) |> as.data.table() |> setnames(old=c("x","y"),new=c("xs","ys"))
DTS[,h:=10]
