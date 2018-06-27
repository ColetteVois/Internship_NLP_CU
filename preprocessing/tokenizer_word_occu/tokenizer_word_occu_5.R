##############################TOKEN 5

library(tokenizers)

tokenizer.word.5 <- function(my.texte) {
  tokens <- tokenize_tweets(paste0(my.texte[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- dim(as.data.frame(tokens))[1]
  return(nb.of.words)
  #717497
}

tokenizer.word.5(original_books)