#############################NORMALIZE 4

library(hunspell)
#attention ici avec meme tokenization de base pour tweet

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

normalize.4 <- function(my.texte) {
  if(tweet == TRUE)#TO DO a mieux faire
  {
    tokens0 <- my.texte %>%
      unnest_tokens(text, text)
    if (DEBUG == TRUE) {tokens0} 
  } else {
    tokens0 <- my.texte
  }
  tokens1 <- text_tokens(tokens0, stemmer = stem_hunspell)
  tokens2 <- unlist(tokens1, recursive=FALSE)
  tokens <- unique(tokens2)
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- length(tokens)
  return(nb.of.words)
  #725056
}

normalize.4(original_books)