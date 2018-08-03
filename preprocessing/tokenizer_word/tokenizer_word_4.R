#' @description Divides the text into its different words. Using "tokenize_words()" function from "tokenizers" package with lowercase = TRUE argument.
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
#' @import tokenizers, dplyr
#' @examples
#' ## library(tokenizers)
#' ## library(dplyr)
#' ## token_word <- tokenizer.word.4(token_sentence[k,], k)

tokenizer.word.4 <- function(my.texte, k) {
  
  #my.texte <- token_sentence[k,]
  
  book_name <- my.texte[2][[1]]
  tokens <- tokenize_words(paste0(my.texte[1]), lowercase = TRUE)
  names(tokens) <- "word"
  tokens <- as_tibble(tokens)
  token_word <- tokens %>% mutate(sentence = k, book = book_name)
  
  return(token_word)
}

#token_word <- tokenizer.word.4(token_sentence[k,], k)