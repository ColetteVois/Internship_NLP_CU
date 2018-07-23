
after.choose.token <- function(my.texte, choose_tokenizer_sentence, choose_tokenizer_word) {
  
  #my.texte <- original_books_bis
  #choose_tokenizer_sentence <- 1
  #choose_tokenizer_word <- 1
  
  token_word <- c()
  
  lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_word_occu/tokenizer_word_occu_%d.R", choose_tokenizer_word), sep="")
  source(lien)
  
  lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_sentence/tokenizer_sentence_%d.R", choose_tokenizer_sentence), sep = "")
  source(lien)

  tokenizer.sentence.i <- sprintf("tokenizer.sentence.%d(my.texte)", choose_tokenizer_sentence)
  token_sentence <- eval(parse(text=tokenizer.sentence.i))
    
  tokenizer.word.i <- sprintf("tokenizer.word.%d(token_sentence[k,],k)", choose_tokenizer_word)
    
  for(k in 1:dim(token_sentence)[1]) {
    #k = 1
    new_token_word <- eval(parse(text=tokenizer.word.i))
    token_word <- dplyr::bind_rows(token_word,new_token_word) 
  }
  
  
  token_word_sort <- token_word %>% arrange(word)
  pre_curseur <- 1
  curseur <- 1
  token_word_freq <- c()
  while(curseur < nrow(token_word_sort)) {
    while(identical(token_word_sort$word[pre_curseur], token_word_sort$word[curseur])) {
      curseur <- curseur + 1
    }
    list_sentence <- unique(token_word_sort[[2]][pre_curseur:(curseur - 1)])
    freq <- curseur - pre_curseur
    token_word_freq1 <- list(word = token_word_sort[[1]][pre_curseur], sentences = list(list_sentence), freq = freq)
    names(token_word_freq1) <- c("word","sentences", "freq")
    token_word_freq <- dplyr::bind_rows(token_word_freq, as_tibble(token_word_freq1))
    pre_curseur <- curseur
    #print(token_word_freq)
  }
  #print(token_word_freq)

  return(c(list(token_sentence),list(token_word),list(token_word_freq)))
}

after.choose.token(original_books_bis, 1, 1) 
