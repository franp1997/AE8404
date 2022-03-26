library(data.table)
library(tint)
library(highcharter)

def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\n \\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})


# Función histograma. 
hist.est <- function(X,h=NULL,breaks=NULL, plot=FALSE){
  # build interval Bj
  if(!is.null(h)){
    # Intervalos definidos por tamaño de paso.
    x0 <- min(X)
    xn <- max(X)
    n <- round((xn-x0)/h,digits = 0)
    
  } else if(!is.null(breaks) & length(breaks)>1){
    # Intervalos definidos por puntos de corte
    n <- length(breaks)
    H <- breaks[2:n] - breaks[1:(n-1)]  
    stopifnot(all(H[1]==H))
    x0 <- min(breaks)
    xn <- max(breaks)
    h <- (xn-x0)/n
    
  } else if(!is.null(breaks) & length(breaks)==1 & breaks>1){
    # Intervalos definidos por cantidad de puntos de corte
    n <- breaks
    h <- (xn-x0)/n
    x0 <- min(X)
  }
  # Construir intervalos
  B <- data.table(j=seq(1,n))[,.(a=x0+(j-1)*h,b=x0+j*h)]
  
  # construir columnas de conteos para cada xi
  C <- matrix(0,nrow = n,ncol = 1)
  for(xi in sort(X)){
    C <- cbind(C,II(xi,B))}
  # 
  C <- rowSums(C)
  # Construir Tabla de intervalos, counts y densidades
  H <- cbind(B,nx=C)
  if(sum(H$nx)!=length(X)) warning("Algunos elementos de X no fueron asignados a ningún bin.")
  if(plot==TRUE){
   N <- H[,.(c=1/2*(a+b))]$c
    barplot(height = H$nx,names.arg=breaks,main="Histogram")
    axis(side=1,at=breaks,tick =TRUE )
  } else return(H)
 
}

# Función Indicadora 
II <- function(x,B){B[,.(c=ifelse(x>=a & x<b,TRUE,FALSE))]
}

# Función de estimación de densidad basada en puntos de corte
fh.est.x0 <- function(x,X=NULL,h=NULL,breaks=NULL){
  
  if(!is.null(h)){
    # Intervalos definidos por tamaño de paso.
    x0 <- min(X)
    xn <- max(X)
    n <- round((xn-x0)/h,digits = 0)
    
  } else if(!is.null(breaks) & length(breaks)>1){
    # Intervalos definidos por puntos de corte
    n <- length(breaks)
    H <- breaks[2:n] - breaks[1:(n-1)]  
    stopifnot(all(H[1]==H))
    x0 <- min(breaks)
    xn <- max(breaks)
    h <- (xn-x0)/n
    
  } else if(!is.null(breaks) & length(breaks)==1 & breaks>1){
    # Intervalos definidos por cantidad de puntos de corte
    n <- breaks
    h <- (xn-x0)/n
    x0 <- min(X)
  }
  # Construir intervalos
  Bj <- data.table(j=seq(1,n))[,.(a=x0+(j-1)*h,b=x0+j*h)]
  
  # construir columnas de conteos para cada xi
  C <- matrix(0,nrow = n,ncol = 1)
  for(xi in sort(X)){
    C <- cbind(C,II(x=xi,B=Bj))}
  # 
  C <- rowSums(C)
  fx <- C/(n*h)
  # # Construir Tabla de intervalos, counts y densidades
  if(sum(C)<length(X)) warning("Algunos elementos de X no fueron asignados a ningún bin.")
  
  if(sum(C)>length(X)) warning("Algunos elementos de Xfueron asignados a más de un bin.")
  # Localizar x en los puntos de corte a,b
  idx <- II(x=x,B=Bj)$c
  return(fx[idx])
}


