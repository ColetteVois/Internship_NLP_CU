#' @description Divides the text into its different words. Using "unnest_tokens()" function from "tidytext".
#' 
#' @param token_sentence[k,] A tibble with two colums and just one row. 
#' token_sentence$sentence is the k th sentence of the text. 
#' token_sentence$book is the part (e.g. chapter, different book...) 
#' of the full text to which this sentence belongs
#' @param k A integer, the number of the sentence in the text
#' @return token_word A tibble with two colums
#' token_word$word is each word of the sentence k, in the same order as in the sentence
#' token_word$sentence is the number of the sentence each wod belongs
#' token_word$book is the name of the book each word belongs
#' 
#' @import tidytext, dplyr
#' @examples
#' ## library(tidytext)
#' ## library(dplyr)
#' ## token_word <- tokenizer.word.1(token_sentence[k,], k)

tokenizer.word.1 <- function(my.texte, k) {
  
  #my.texte <- token_sentence[k,]
  
  my_texte <- my.texte[1]
  book_name <- my.texte[2][[1]]
  tidy_books <- my_texte %>%
    unnest_tokens(word, sentence)
  token_word <- tidy_books %>% mutate(sentence = k, book = book_name)

  return(token_word)
}

#token_word <- tokenizer.word.1(token_sentence[k,], k)