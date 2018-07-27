library("corpus")
tweet = FALSE
#attention ici avec meme tokenization de base pour tweet

normalize.3 <- function(my.texte) {
  
  # my.texte <- token_word_freq
  
  my_texte <- my.texte[1]
  names(my_texte) <- "text"
  
  #TO DO a mieux faire
  if(tweet == TRUE)  {
    tokens0 <- my.texte %>%
      unnest_tokens(text, text)
    if (DEBUG == TRUE) {tokens0} 
  } else {
    tokens0 <- my_texte
  }
  tokens1 <- text_tokens(tokens0, stemmer = "en")
  tokens2 <- unlist(tokens1, recursive=FALSE)
  tokens3 <- sort(tokens2)
  
  listfiles <- unique(tokens3)
  pre_curseur <- 1
  curseur <- 1
  col_word <- c()
  col_sentence <- c()
  col_freq <- c()
  for(word in 1:length(listfiles)) {
    
    #word = 13
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
    
  }
  
  # if (DEBUG == TRUE) {tokens} 
  token_word_stem <- tibble(word = col_word, sentences = col_sentence, freq = col_freq)
  
  return(token_word_stem)

}

#normalize.3(token_word_freq)
