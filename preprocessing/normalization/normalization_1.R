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
#' @import tokenizers
#' @examples
#' ## library(tokenizers)
#' ## token_word_stem <- normalize.1(token_word_freq)

normalize.1 <- function(my.texte) {
  
  #my.texte <- token_word_freq
  
  tokens1 <- tokenize_word_stems(paste0(my.texte[[1]]))
  tokens2 <- unlist(tokens1, recursive=FALSE) 
  tokens3 <- sort(tokens2)

  listfiles <- unique(tokens3)
  pre_curseur <- 1
  curseur <- 1
  col_word <- c()
  col_sentence <- c()
  col_freq <- c()
  for(word in 1:length(listfiles)) {
    #word = 27
    freq <- 0
    sentence <- c()
    while(identical(listfiles[word], tokens3[curseur])) {
      freq <- freq + my.texte[curseur,]$freq
      sentence <- c(sentence, unlist(my.texte[curseur,]$sentences))
      curseur <- curseur + 1
    }
    col_word <- c(col_word, tokens3[(curseur-1)])
    col_sentence <- c(col_sentence, list(sentence))
    col_freq <- c(col_freq,freq)
    pre_curseur <- curseur
    col_word
  }
  
  token_word_stem <- tibble(word = col_word, sentences = col_sentence, freq = col_freq)

  return(token_word_stem)
}

#token_word_stem <- normalize.1(token_word_freq)
