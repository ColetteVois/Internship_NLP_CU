#------------------------------------NOMALIZATION----------------------------#

#########################NORMALIZE 1

library(tokenizers)

normalize.1 <- function(my.texte) {
  tokens1 <- tokenize_word_stems(paste0(my.texte[1]))
  tokens2 <- unlist(tokens1, recursive=FALSE)
  tokens <- unique(tokens2)
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- dim(as.data.frame(tokens))
  return(nb.of.words[1])
  #725056
}

normalize.1(original_books)