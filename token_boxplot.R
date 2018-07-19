
token.boxplot <- function(original_books_select) {


  n.tokenizer.sentence <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_sentence/", sep = "")))
  n.tokenizer.word.occu <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_word_occu/", sep=""))) - 1
  
  nb.of.sentence <- c()
  nb.of.word.occu <- c()
  nb.of.word.type <- c()
  
  token_word <- c()
  
  #fait n.tokenizer.sentence tokenisations de senctence differntes
  for (i in 1:n.tokenizer.sentence){
    
    #i = 1 #2,3
    lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_sentence/tokenizer_sentence_%d.R", i), sep = "")
    source(lien)
    tokenizer.sentence.i <- sprintf("tokenizer.sentence.%d(original_books_select)", i)
    if (DEBUG == TRUE) { print(tokenizer.sentence.i) }
    token_sentence <- eval(parse(text=tokenizer.sentence.i))#[[1]][1]
    if (DEBUG == TRUE) { print(token_sentence) }
    nb.of.sentence[i] <- dim(token_sentence)[1]
    if (DEBUG == TRUE) { print(nb.of.sentence) }
    
    for (j in 1:n.tokenizer.word.occu){
      
      token_word <- c()
      #j = 3 #3,4,5
      if(i==1) {
        lien <- paste(my_path,sprintf("/Intership_NLP_CU/preprocessing/tokenizer_word_occu/tokenizer_word_occu_%d.R", j), sep="")
        source(lien)
      }
      tokenizer.word.i <- sprintf("tokenizer.word.%d(token_sentence[k,])", j)
      if (DEBUG == TRUE) { print(tokenizer.word.i) }
      
      for(k in 1:nb.of.sentence[i]) {
        #k = 2  #2,3,4...15773
        if (DEBUG == TRUE) { token_sentence[k,] }
        new_token_word <- eval(parse(text=tokenizer.word.i))
        if (DEBUG == TRUE) { new_token_word }
        token_word <- dplyr::bind_rows(token_word,new_token_word) #TODO mettre bout à bout des matrice
        if (DEBUG == TRUE) { token_word }
        #nb.of.word.occu[i]  <- sum(token_word[2])
        #nb.of.word.type[i]  <- dim(token_word[2])[1] 
      }
      ###TO DO NOMBREs de mots a verifier 464,193, 464,194
      nb.of.word.occu[j+(i-1)*n.tokenizer.word.occu]  <- nrow(token_word)
      
      ## TO DO a mettre 
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
        token_word_freq
      }
      nb.of.word.type[j+(i-1)*n.tokenizer.word.occu]  <- nrow(token_word_freq)
      
    }
    
  }

}
