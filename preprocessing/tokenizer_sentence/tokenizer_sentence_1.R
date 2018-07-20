library(tokenizers)
library(tibble)
library(pracma)
library(dplyr)

DEBUG = TRUE

tokenizer.sentence.1 <- function(my.texte) {
  
  #my.texte <- original_books_bis
  listfiles <- unique(my.texte[[2]])
  pre_curseur <- 1
  curseur <- 1
  tokens <- c()
  col_2 <- c()
  for(docu in 1:length(listfiles)) {

    #docu = 1
    while(identical(listfiles[docu], my.texte[[2]][curseur])) {
      curseur <- curseur + 1
    }
    new_token <- tokenize_sentences(stri_encode(paste0(as_tibble(my.texte[[1]][pre_curseur:(curseur-1)])), "", "UTF-8"), lowercase = TRUE)[[1]]
    tokens <- c(tokens, new_token)
    col_2 <- c(col_2, rep(toString(listfiles[docu]), length(new_token)))
    pre_curseur <- curseur
    
  }
  
  #tokens1 <- tokenize_sentences(paste0(as_tibble(original_books[[1]])), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens} 
  tokens <- as_tibble(tokens)
  tokens <- tokens %>% mutate(book = col_2)
  names(tokens) <- c("sentence","book")
  return(tokens)
}

#tokenizer.sentence.1(original_books)