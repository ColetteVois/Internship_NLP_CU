#This function is a modulo function, but instead of having x %% y = 0, it has x %% y = y
modulo.not.null <- function(x,y) {
  if(x%%y == 0){
    mod <- y
  }
  else{
    mod <- x%%y
  }
  return(mod)
}