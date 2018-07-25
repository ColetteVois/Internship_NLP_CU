library(tokenizers)

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
      freq <- freq + token_word_freq[curseur,]$freq
      sentence <- c(sentence, unlist(token_word_freq[curseur,]$sentences))
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
