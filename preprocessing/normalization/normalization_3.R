#################################NORMALIZE 3
library("corpus")
tweet = TRUE
#attention ici avec meme tokenization de base pour tweet

normalize.3 <- function(my.texte) {
  if(tweet == TRUE)#TO DO a mieux faire
  {
    tokens0 <- my.texte %>%
      unnest_tokens(text, text)
    if (DEBUG == TRUE) {tokens0} 
  } else {
    tokens0 <- my.texte
  }
  tokens1 <- text_tokens(tokens0[1], stemmer = "en")
  tokens2 <- unlist(tokens1, recursive=FALSE)
  tokens <- unique(tokens2)
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- length(tokens)
  return(nb.of.words)
  #725056
}

normalize.3(original_books[1])