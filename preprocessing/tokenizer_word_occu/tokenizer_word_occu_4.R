###############################TOKEN 4

library(tokenizers)

tokenizer.word.4 <- function(my.texte) {
  tokens <- tokenize_words(paste0(my.texte[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- dim(as.data.frame(tokens))[1]
  return(nb.of.words)
  #725056
}

tokenizer.word.4(original_books)