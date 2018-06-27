#-----------------------------------------SENTENCE---------------------------#

############################TOKEN 1

library(tokenizers)

DEBUG = TRUE

tokenizer.sentence.1 <- function(my.texte) {
  tokens <- tokenize_sentences(paste0(my.texte[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- dim(as.data.frame(tokens))[1]
  print(nb.of.words)
  return(nb.of.words)
  #31396
}

#tokenizer.sentence.1(original_books)