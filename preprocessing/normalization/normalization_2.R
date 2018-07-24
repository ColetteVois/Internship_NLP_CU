library(hunspell)
#attention ici avec meme tokenization de base pour tweet
tweet = FALSE

stem_hunspell <- function(my.texte) {
  
  # look up the term in the dictionary
  tokens <- unlist(my.texte, recursive=FALSE)
  stems <- hunspell_stem(tokens)[[1]]
  #print(stems)
  if (length(stems) == 0) { # if there are no stems, use the original term
    stem <- my.texte
  } else { # if there are multiple stems, use the last one
    stem <- stems[[length(stems)]]
  }
  
  stem
  
}

normalize.2 <- function(my.texte) {
  
  #my.texte <- token_word_freq
  
  my_texte <- my.texte[1]
  names(my_texte) <- "text"
  
  #TO DO a mieux faire
  if(tweet == TRUE)  {
    tokens0 <- my_texte %>%
      unnest_tokens(text, text)
    if (DEBUG == TRUE) {tokens0} 
  } else {
    tokens0 <- my_texte
  }
  tokens1 <- text_tokens(tokens0, stemmer = stem_hunspell)
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

#normalize.2(token_word_freq)
