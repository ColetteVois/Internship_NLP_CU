wordcloud.data.func <- function(my.texte_tokenized_word_sentence, list_sentences, word_selected){
  withProgress(expr={local_list <- c()
  for(i in 1:NROW(my.texte_tokenized_word_sentence)){
    inc = 1/(NROW(my.texte_tokenized_word_sentence))
    incProgress(inc)
    for(j in list_sentences){
      if(identical(my.texte_tokenized_word_sentence$sentence[i], strtoi(j)) & !(identical(my.texte_tokenized_word_sentence$word[i],word_selected))){
        local_list <- c(local_list, my.texte_tokenized_word_sentence[1]$word[i])
      }
    }
  }
  local_list <- data.frame(word=local_list)
  }, message= "cr")

    #Sort data_tokenized_word by alphabetical order
    token_word_sort <- local_list %>% arrange(word)
    pre_curseur <- 1
    curseur <- 1
    token_word_freq <- c()
    while(curseur < nrow(token_word_sort)) {
      while(identical(token_word_sort$word[pre_curseur], token_word_sort$word[curseur])) {
        curseur <- curseur + 1
      }
      freq <- curseur - pre_curseur
      token_word_freq1 <- list(word = token_word_sort[[1]][pre_curseur], freq = freq)
      names(token_word_freq1) <- c("word", "freq")
      token_word_freq <- dplyr::bind_rows(token_word_freq, as_tibble(token_word_freq1))
      pre_curseur <- curseur
    }
    return(token_word_freq)
}