modulo.not.null <- function(x,y) {
  if(x%%y == 0){
    mod <- y
  }
  else{
    mod <- x%%y
  }
  return(mod)
}