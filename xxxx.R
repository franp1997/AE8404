Betas <- function(data=NULL,y=NULL,x=NULL){
  if(!is.null(data) & is.character(y)){ 
    Y <- data[[y]] 
    XCOLS <- colnames(data)[colnames(data)!=y]
    x <- data[,..XCOLS] |> as.matrix()
  } 
  else if(!is.null(x) & is.vector(y)){ 
    Y <- y
    if(!is.matrix(x)){x <- as.matrix(x)}
  } else error()
  X <- cbind(1,x)
  B <- solve(t(X)%*%X)%*%t(X)%*%Y |> t() |> as.data.table()
  p <- ncol(x)
  ID <- paste0("b",seq(0,p)) #b0,b1,..bp
  setnames(B,new=ID)
  return(B)
}
