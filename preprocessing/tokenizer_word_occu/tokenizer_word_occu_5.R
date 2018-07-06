##############################TOKEN 5

library(tokenizers)

tokenizer.word.5 <- function(my.texte) {
  
  tokens <- tokenize_tweets(paste0(my.texte[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens}
  names(tokens) <- "word"
  tokens <- as_tibble(tokens)
  tidy_books_count = tokens %>%
    count(word, sort = TRUE) 
  #nb.of.words <- dim(as.data.frame(tokens))[1]
  return(tidy_books_count)

}

tokenizer.word.5(original_books)