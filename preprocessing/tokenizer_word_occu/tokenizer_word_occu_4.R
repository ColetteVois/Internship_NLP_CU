library(tokenizers)

tokenizer.word.4 <- function(my.texte) {
  
  my.texte <- token_sentence[k,]
  book_name <- my.texte[2][[1]]
  tokens <- tokenize_words(paste0(my.texte[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens} 
  names(tokens) <- "word"
  tokens <- as_tibble(tokens)
  tidy_books <- tokens %>% mutate(sentence = k, book = book_name)
  return(tidy_books)

}

#tokenizer.word.4(original_books)