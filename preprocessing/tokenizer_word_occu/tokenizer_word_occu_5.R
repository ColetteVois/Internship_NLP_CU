library(tokenizers)

tokenizer.word.5 <- function(my.texte,k) {
  
  #my.texte <- token_sentence[k,]
  book_name <- my.texte[2][[1]]
  tokens <- tokenize_tweets(paste0(my.texte[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens}
  names(tokens) <- "word"
  tokens <- as_tibble(tokens)
  tidy_books <- tokens %>% mutate(sentence = k, book = book_name)
  return(tidy_books)

}

#tokenizer.word.5(original_books,k)