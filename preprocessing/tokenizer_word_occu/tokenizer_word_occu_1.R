library("tidytext")
#------------------------------------WORD------------------------------------#

#############################TOKEN 1

tokenizer.word.1 <- function(my.texte) {
  tidy_books <- my.texte %>%
    unnest_tokens(word, text)
  if (DEBUG == TRUE) {tidy_books}
  tidy_books_count = tidy_books %>%
    count(word, sort = TRUE) 
  if (DEBUG == TRUE) {tidy_books_count}
  nb.of.words <- sum(tidy_books_count[2])
  nb.of.types <- dim(tidy_books_count[2])[1]
  return(nb.of.words)#c(nb.of.words, nb.of.types))
  #725055-14520
}

tokenizer.word.1(original_books)