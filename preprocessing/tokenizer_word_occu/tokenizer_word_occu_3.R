library("tm")

tokenizer.word.3 <- function(my.texte) {
  
  my.texte <- token_sentence[k,]
  book_name <- my.texte[2][[1]]
  tokens <- Boost_tokenizer(my.texte[1])
  if (DEBUG == TRUE) {tokens} 
  tokens <- as_tibble(tokens)
  names(tokens) <- "word"
  tidy_books <- tokens %>% mutate(sentence = k, book = book_name)
  return(tidy_books)
  
}

#tokenizer.word.3(original_books)