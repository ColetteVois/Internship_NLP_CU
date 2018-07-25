
token.boxplot <- function(my.texte) {

  #my.texte <- original_books_bis
  
  n.tokenizer.sentence <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_sentence/", sep = "")))
  n.tokenizer.word.occu <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_word_occu/", sep=""))) - 1
  n.normalization <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/normalization/", sep=""))) - 1

  nb.of.sentence <- c()
  nb.of.word.occu <- c()
  nb.of.word.type <- c()
  nb.of.normalization <- c()
  
  #fait n.tokenizer.sentence tokenisations de senctence differntes
  for (i in 1:n.tokenizer.sentence){
    
    #i = 2 #2,3
    lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_sentence/tokenizer_sentence_%d.R", i), sep = "")
    source(lien)
    tokenizer.sentence.i <- sprintf("tokenizer.sentence.%d(my.texte)", i)
    if (DEBUG == TRUE) { print(tokenizer.sentence.i) }
    token_sentence <- eval(parse(text=tokenizer.sentence.i))#[[1]][1]
    if (DEBUG == TRUE) { print(token_sentence) }
    nb.of.sentence[i] <- dim(token_sentence)[1]
    if (DEBUG == TRUE) { print(nb.of.sentence) }
    
    for (j in 1:n.tokenizer.word.occu){
      
      token_word <- c()
      #j = 5 #3,4,5
      if(i==1) {
        lien <- paste(my_path,sprintf("/Intership_NLP_CU/preprocessing/tokenizer_word_occu/tokenizer_word_occu_%d.R", j), sep="")
        source(lien)
      }
      tokenizer.word.i <- sprintf("tokenizer.word.%d(token_sentence[k,],k)", j)
      if (DEBUG == TRUE) { print(tokenizer.word.i) }
      
      for(k in 1:nb.of.sentence[i]) {
        #k = 2  #2,3,4...15773
        if (DEBUG == TRUE) { token_sentence[k,] }
        new_token_word <- eval(parse(text=tokenizer.word.i))
        if (DEBUG == TRUE) { new_token_word }
        token_word <- dplyr::bind_rows(token_word,new_token_word) #TODO mettre bout � bout des matrice
        if (DEBUG == TRUE) { token_word }
      }
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
      
    
    
      #normalization
      for(l in 1:n.normalization) {
        
        #l = 1
        if(j==1) {
          lien <- paste(my_path,sprintf("/Intership_NLP_CU/preprocessing/normalization/normalization_%d.R", l), sep="")
          source(lien)
        }
        normalization.i <- sprintf("normalize.%d(token_word_freq)", l)
        token_word_stem <- eval(parse(text=normalization.i))
        nb.of.normalization[l+(j-1)*n.normalization+(i-1)*n.tokenizer.word.occu*n.normalization]  <- nrow(token_word_stem)
        #print(c(i,j,l))
        #print(token_word_stem)
      }
    
    }
  }
  
  return(c(list(nb.of.sentence),list(nb.of.word.occu),list(nb.of.word.type),list(nb.of.normalization)))

}

token.boxplot(original_books_bis)