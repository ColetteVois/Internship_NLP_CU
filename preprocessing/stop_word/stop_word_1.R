library(tidytext)

stop.word.1 <- function(my.texte) {
  
  #my.texte <- token_word_stem
  
  token_word_stop <- my.texte %>%
    anti_join(get_stopwords(),by = "word")

  return(token_word_stop)

}

#token_word_stop <- stop.word.1(token_word_stem)
