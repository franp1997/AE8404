library(tint)
library(highcharter)
library(data.table)


# Función Indicadora y contadora
II <- function(X,B){
  sapply(X, function(xi){apply(B,1,function(bj){xi %inrange% bj})},simplify = TRUE) |> rowSums()
  
}

hist.estimate <- function(X,breaks=NULL,h=NULL){
  # Construir intervalos Bj
  x0 <- min(X)-h
  xn <- max(X)+h
  n <- floor((xn-x0)/h)
  C <- vector(mode="integer",length = n)
  B <- matrix(0.0,nrow=n,ncol=2)
  for(j in seq(1,n)){
    # Bj <- c(x0+(j-1)*h,x0+j*h)
    B[j,1] <- x0+(j-1)*h
    B[j,2] <- x0+j*h
    for(xi in X){
      # C[j] <- C[j] + ifelse(xi<=Bj[2]&xi>=Bj[1],1,0)
      C[j] <- C[j] + ifelse(xi<=B[j,2]&xi>=B[j,1],1,0)
    }
  }
  return(list(count=C,Bj=B))
}
# Xi=runif(n=40,min=80,max=200)

fh.dens <- function(X,h){
  H <- fh.count(X,h)
  n <- length(C)
  fh <- C/(h*n)
  return(fh)
}
