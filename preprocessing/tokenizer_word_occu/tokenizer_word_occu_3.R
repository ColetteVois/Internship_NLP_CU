###############################TOKEN 3
library("tm")

tokenizer.word.3 <- function(my.texte) {
  
  tokens <- Boost_tokenizer(original_books[1])
  if (DEBUG == TRUE) {tokens} 
  tokens <- as_tibble(tokens)
  names(tokens) <- "word"
  tidy_books_count = tokens %>%
    count(word, sort = TRUE) 
  #nb.of.words <- length(tokens)
  return(tidy_books_count)
  #728907
  
}

tokenizer.word.3(original_books)