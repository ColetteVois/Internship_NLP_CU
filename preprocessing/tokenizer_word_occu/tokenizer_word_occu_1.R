library("tidytext")

tokenizer.word.1 <- function(my.texte,k) {
  
  #my.texte <- token_sentence[k,]
  my_texte <- my.texte[1]
  book_name <- my.texte[2][[1]]
  tidy_books <- my_texte %>%
    unnest_tokens(word, sentence)
  if (DEBUG == TRUE) {tidy_books}
  tidy_books <- tidy_books %>% mutate(sentence = k, book = book_name)#rep(book_name,dim(tidy_books)[1]))

  return(tidy_books)

}

#tokenizer.word.1(original_books)