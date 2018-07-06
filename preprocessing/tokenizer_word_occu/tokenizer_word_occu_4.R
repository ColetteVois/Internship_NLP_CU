###############################TOKEN 4

library(tokenizers)

tokenizer.word.4 <- function(my.texte) {
  
  tokens <- tokenize_words(paste0(original_books[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens} 
  names(tokens) <- "word"
  tokens <- as_tibble(tokens)
  tidy_books_count = tokens %>%
    count(word, sort = TRUE) 
  #nb.of.words <- dim(as.data.frame(tokens))[1]
  return(tidy_books_count)

}

tokenizer.word.4(original_books)