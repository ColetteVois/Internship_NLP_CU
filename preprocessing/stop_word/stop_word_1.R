#' @description Normalize the words of the text. Using "anti_join()" function from "tidytext" package with  get_stopwords(), by = "word" arguments.
#' 
#' @param token_word_stem A tibble with four columns. 
#' token_word_stem$word are the normalize form of words of the text in alphabetical order occuring just ones. 
#' token_word_stem$sentences is the list of numbers of sentences (line of the sentence in token_sentence) in which each normalize word appear.
#' token_word_stem$freq is the frequence each normalize word appears in the text.
#' @return token_word_stop A tibble with three columns.
#' token_word_stop$word are the normalize form of words of the text in alphabetical order occuring just ones minus the most current words.
#' token_word_stop$sentences  is the list of numbers of sentences (line of the sentence in token_sentence) in which each word appear.
#' token_word_stop$freq is the frequence each word appears in the text.
#' 
#' @import tidytext
#' @examples
#' ## library(tidytext)
#' ## token_word_stop <- stop.word.1(token_word_stem)

stop.word.1 <- function(my.texte) {
  
  #my.texte <- token_word_stem
  
  token_word_stop <- my.texte %>%
    anti_join(get_stopwords(),by = "word")

  return(token_word_stop)
}

# token_word_stop <- stop.word.1(token_word_stem)