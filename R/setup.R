library(data.table)
library(tint)
library(ggplot2)
library(data.table)
library(ggthemes)
library(flextable)
# Rmarkdown ----
knitr::opts_chunk$set(out.width="50%",fig.align = "center",eval=TRUE, echo = FALSE, warning = FALSE, size = "small")
def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\n \\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})
# Identificar output  ----
out_type <- knitr::opts_knit$get("rmarkdown.pandoc.to")


use_df_printer()
set_flextable_defaults(
  font.size = 10, font.family="Helvetica",border.color = "gray",
  padding.bottom = 3,   padding.top = 3,
  padding.left = 4,  padding.right = 4,
  post_process_html = function(x){
    # theme_booktabs(x)  |>
    theme_vanilla(x) |>
      set_table_properties(layout = "autofit") |>
      autofit() |>
      align(align="center",part = "header") |>
      bold(part = "header")  |> 
      fontsize(size = 10, part = "header")|> 
      fontsize(size = 10, part = "body")
  },
  post_process_pdf = function(x){
    # theme_booktabs(x)  |>
      theme_vanilla(x) |>
      set_table_properties(layout = "autofit") |>
      autofit() |>
      align(align="center",part = "header") |>
      bold(part = "header")  |>
      fontsize(size = 9, part = "header")|> 
      fontsize(size = 9, part = "body")
  },
  post_process_docx = function(x){
    theme_vanilla(x) |>
      set_table_properties(layout = "autofit") |>
      autofit() |>
      align(align="center",part = "header") |>
      bold(part = "header")  |>
      fontsize(size = 10, part = "header")|> 
      fontsize(size = 9, part = "body")
  }
)



# Función Indicadora   ----
II <- function(X,B){
  # X scalar B: Table (intercal)
  return(B[,.(c=ifelse(X>=a & X<b,TRUE,FALSE))])
}


# Función de estimación de densidad basada en puntos de corte  ----
fx.est.x0 <- function(x=NULL,X=NULL,h=NULL,x0=NULL,xn=NULL,breaks=NULL){
  if(is.null(x0)){ x0 <- min(X)}
  if(is.null(xn)){ xn <- max(X)}
  if(!is.null(h)){
    # Intervalos definidos por tamaño de paso.
    n <- round((xn-x0)/h,digits = 0)
  } else if(!is.null(breaks) & length(breaks)>1){
    # Intervalos definidos por puntos de corte
    n <- length(breaks)
    H <- breaks[2:n] - breaks[1:(n-1)]  
    stopifnot(all(H[1]==H))
    h <- sort(breaks)[2]-sort(breaks)[1]
    
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
  for(Xi in sort(X)){
    C <- cbind(C,II.Bj(X=Xi,B=Bj))}
  # 
  C <- rowSums(C)
  fx <- C/(n*h)
  H <- cbind(Bj,C,fx)
  # # Construir Tabla de intervalos, counts y densidades
  if(sum(C)<length(X)) warning("Algunos elementos de X no fueron asignados a ningún bin.")

  if(sum(C)>length(X)) warning("Algunos elementos de X fueron asignados a más de un bin. Error de código")

  if(is.null(x)){return(H)}
  
  if(!is.null(x) & length(x)==1){
    # Localizar x en los puntos de corte a,b
    idx <- II(X=x,B=Bj)$c
    return(fx[idx])
  } 
 
  
}

