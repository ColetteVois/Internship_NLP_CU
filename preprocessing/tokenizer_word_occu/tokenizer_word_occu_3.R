###############################TOKEN 3
library("tm")

tokenizer.word.3 <- function(my.texte) {
  tokens <- Boost_tokenizer(my.texte[1])
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- length(tokens)
  return(nb.of.words)
  #728907
}

tokenizer.word.3(original_books)