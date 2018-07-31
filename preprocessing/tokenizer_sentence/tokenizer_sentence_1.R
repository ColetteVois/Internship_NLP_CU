#' @description Divides the text into its different sentences. Using "tokenize_sentences()" function from "tokenizers" library.
#' 
#' @param original_books_bis A tibble with two colums. 
#' original_book$text is the lines of the text. 
#' original_book$book is the part (e.g. chapter, different book...) 
#' of the full text to which the lines belong
#' @return token_sentence A tibble with two colums. 
#' token_sentence$sentence is each sentence of the text. 
#' token_sentence$book is the part (e.g. chapter, different book...) 
#' of the full text to which the sentence belong
#' 
#' @import tokenizers, tibble, pracma, dplyr, stringi
#' #' @examples
#' ## library(tokenizers)
#' ## library(tibble)
#' ## library(pracma)
#' ## library(dplyr)
#' ## library(stringi)
#' ## token_sentence <- tokenizer.sentence.1(original_books_bis)

tokenizer.sentence.1 <- function(my.texte) {
  
  #my.texte <- original_books_bis
  
  listfiles <- unique(my.texte[[2]])
  pre_curseur <- 1
  curseur <- 1
  token_sentence <- c()
  col_2 <- c()
  for(docu in 1:length(listfiles)) {

    #docu = 1
    while(identical(listfiles[docu], my.texte[[2]][curseur])) {
      curseur <- curseur + 1
    }
    new_token <- tokenize_sentences(stri_encode(paste0(as_tibble(my.texte[[1]][pre_curseur:(curseur-1)])), "", "UTF-8"), lowercase = TRUE)[[1]]
    token_sentence <- c(token_sentence, new_token)
    col_2 <- c(col_2, rep(toString(listfiles[docu]), length(new_token)))
    pre_curseur <- curseur
    
  }
  
  token_sentence <- as_tibble(token_sentence)
  token_sentence <- token_sentence %>% mutate(book = col_2)
  names(token_sentence) <- c("sentence","book")
  
  return(token_sentence)
  
}

#token_sentence <- tokenizer.sentence.1(original_books)