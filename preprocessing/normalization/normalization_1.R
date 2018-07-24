library(tokenizers)

normalize.1 <- function(my.texte) {
  
  #my.texte <- token_word_freq
  
  tokens1 <- tokenize_word_stems(paste0(my.texte[[1]]))
  tokens2 <- unlist(tokens1, recursive=FALSE) 

  listfiles <- unique(tokens2)
  pre_curseur <- 1
  curseur <- 1
  col_word <- c()
  col_sentence <- c()
  col_freq <- c()
  for(word in 1:length(listfiles)) {
    
    #word = 13
    freq <- 0
    sentence <- c()
    while(identical(listfiles[word], tokens2[curseur])) {
      freq <- freq + token_word_freq[curseur,]$freq
      sentence <- c(sentence, unlist(token_word_freq[curseur,]$sentences))
      curseur <- curseur + 1
    }
    col_word <- c(col_word, tokens2[(curseur-1)])
    col_sentence <- c(col_sentence, list(sentence))
    col_freq <- c(col_freq,freq)
    pre_curseur <- curseur
    
  }
  
  if (DEBUG == TRUE) {tokens} 
  token_word_stem <- tibble(word = col_word, sentences = col_sentence, freq = col_freq)

  return(token_word_stem)
  
}

#normalize.1(token_word_freq)
