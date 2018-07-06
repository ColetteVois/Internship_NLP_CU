library(tokenizers)
library(tibble)

DEBUG = TRUE

tokenizer.sentence.1 <- function(my.texte) {
  
  tokens <- tokenize_sentences(paste0(my.texte[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens} 
  names(tokens) <- "sentence"
  tokens <- as_tibble(tokens)
  #nb.of.words <- dim(as.data.frame(tokens))[1]
  #if (DEBUG == TRUE) { print(nb.of.words) }
  return(tokens)
}

#tokenizer.sentence.1(original_books)