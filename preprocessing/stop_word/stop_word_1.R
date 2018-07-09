library(tidytext)

stop.word.1 <- function(my.texte) {
  
  cleaned_books <- my.texte %>%
    anti_join(get_stopwords(),by = "word")
  if (DEBUG == TRUE) {cleaned_books} 
  #cleaned_books_count = cleaned_books %>%
  #  count(word, sort = TRUE)
  #if (DEBUG == TRUE) {cleaned_books_count} 
  #nb.of.words <- sum(cleaned_books_count[2])
  return(cleaned_books)

}

#stop.word.1(original_books)