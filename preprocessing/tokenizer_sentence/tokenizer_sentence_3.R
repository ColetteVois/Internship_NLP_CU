#' @description Divides the text into its different sentences. Using "tokens()" function from "quanteda" library with what = "sentence", remove_numbers = FALSE, remove_punct = FALSE, remove_symbols = FALSE, remove_separators = TRUE,remove_twitter = FALSE, remove_hyphens = FALSE, remove_url = FALSE,ngrams = 1L, skip = 0L, concatenator = "_",verbose = quanteda_options("verbose"), include_docvars = TRUE arguments.
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
#' @import quanteda
#' #' @examples
#' ## library("quanteda")
#' ## library(dplyr)
#' ## token_sentence <- tokenizer.sentence.3(original_books_bis)



tokenizer.sentence.3.bis <- function(my.texte) {
  
  #my.texte <- original_books[[1]][pre_curseur:curseur]
  #token <- unlist(original_books, recursive=FALSE)
  
  sentence <- tokens(my.texte, what = "sentence", remove_numbers = FALSE, remove_punct = FALSE,
                     remove_symbols = FALSE, remove_separators = TRUE,
                     remove_twitter = FALSE, remove_hyphens = FALSE, remove_url = FALSE,
                     ngrams = 1L, skip = 0L, concatenator = "_",
                     verbose = quanteda_options("verbose"), include_docvars = TRUE)
  sentences <- as_tibble(unlist(sentence))
  names(sentences) <- "sentence"

  return(sentences)
}

tokenizer.sentence.3 <- function(my.texte) {
  
  #my.texte <- original_books
  
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
    new_token <- tokenizer.sentence.3.bis(my.texte[[1]][pre_curseur:curseur])[[1]]
    token_sentence <- c(token_sentence, new_token)
    col_2 <- c(col_2, rep(toString(listfiles[docu]), length(new_token)))
    pre_curseur <- curseur
  }
  
  token_sentence <- as_tibble(token_sentence)
  token_sentence <- token_sentence %>% mutate(book = col_2)
  names(token_sentence) <- c("sentence","book")
  
  return(token_sentence)
}

#tokenizer.sentence.3(original_books_bis)