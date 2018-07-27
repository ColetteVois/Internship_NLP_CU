library("quanteda")

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
  #nb.of.words <-   length(sentence)
  #print(nb.of.words)
  return(sentences)

}

tokenizer.sentence.3 <- function(my.texte) {
  
  #my.texte <- original_books
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
    new_token <- tokenizer.sentence.3.bis(my.texte[[1]][pre_curseur:curseur])[[1]]
    tokens <- c(tokens, new_token)
    col_2 <- c(col_2, rep(toString(listfiles[docu]), length(new_token)))
    pre_curseur <- curseur
    
  }
  
  tokens <- as_tibble(tokens)
  tokens <- tokens %>% mutate(book = col_2)
  names(tokens) <- c("sentence","book")
  return(tokens)
}



#tokenizer.sentence.3(original_books[1])