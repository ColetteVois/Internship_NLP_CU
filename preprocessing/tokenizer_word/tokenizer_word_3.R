#' @description Divides the text into its different words. Using "Boost_tokenizer()" function from "tm" package.
#' 
#' @param token_sentence[k,] A tibble with two columns and just one row. 
#' token_sentence$sentence is the k th sentence of the text. 
#' token_sentence$book is the part (e.g. chapter, different book...) 
#' of the full text to which this sentence belongs
#' @param k A integer, the number of the sentence in the text
#' @return token_word A tibble with three columns
#' token_word$word is each word of the sentence k, in the same order as in the sentence
#' token_word$sentence is the number of the sentence each wod belongs
#' token_word$book is the name of the book each word belongs
#' 
#' @import tm, dplyr
#' @examples
#' ## library("tm")
#' ## library(dplyr)
#' ## token_word <- tokenizer.word.3(token_sentence[k,], k)


tokenizer.word.3 <- function(my.texte, k) {
  
  #k = 264
  #my.texte <- token_sentence[k,]
  
  book_name <- my.texte[2][[1]]
  tokens <- Boost_tokenizer(my.texte[1])
  tokens <- as_tibble(tokens)
  names(tokens) <- "word"
  token_word <- tokens %>% mutate(sentence = k, book = book_name)
  
  return(token_word)
}

#token_word <- tokenizer.word.3(token_sentence[k,], k)
