library(tokenizers)

tokenizer.word.5 <- function(my.texte,k) {
  
  #k = 264
  #my.texte <- token_sentence[k,]
  
  book_name <- my.texte[2][[1]]
  #if their is just one word tokenize_tweets do not works
  if(length(Boost_tokenizer(my.texte[1])[[1]])>1) {
    #print("if")
    tokens <- tokenize_tweets(paste0(my.texte[1]), lowercase = TRUE)
  }  else{
    #print("else")
    tokens <- tokenize_words(paste0(my.texte[1]), lowercase = TRUE)
  }
  #tokens <- tokenize_tweets(paste0(my.texte[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens}
  names(tokens) <- "word"
  tokens <- as_tibble(tokens)
  tidy_books <- tokens %>% mutate(sentence = k, book = book_name)
  
  return(tidy_books)

}

#tokenizer.word.5(token_sentence[k,],k)
