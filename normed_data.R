################################ USER CHANGE ############################################

#write your path to go to your file
#my_path <- "C:/Users/Projet/Intership_NLP_CU"
# my_path <- "C:/Users/Projet"
#choose which data you want to load
choose_load_data <- 1
DEBUG = TRUE

################################### LOAD DATA ###########################################

source(paste(my_path, sprintf("/Intership_NLP_CU/load_data/load_data_%d.R", choose_load_data), sep = ""))
load.data.i <- sprintf("load.data.%d()", choose_load_data)
original_books <- eval(parse(text=load.data.i))
original_books <- original_books %>% mutate(rowname = 1:nrow(original_books))
original_books_bis <- original_books[1:400,]
#Removing the spaces from the column book. I had to change the type to character in order to change the column and 
#then re change it in factor to let it as it was before.
count = 1
original_books_bis$book <- as.character(original_books_bis$book)
for(i in original_books_bis$book){
  original_books_bis$book[count] <- gsub(" ", "",i)
  count = count +1
}
original_books_bis$book <- as.factor(original_books_bis$book)


n <- NROW(original_books_bis)

################################# TOKENIZER SENTENCE_WORD ###################################

n.tokenizer.sentence <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_sentence/", sep = "")))
n.tokenizer.word.occu <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_word_occu/", sep=""))) - 1

nb.of.sentence <- c()
nb.of.word.occu <- c()
nb.of.word.type <- c()

token_word <- c()

for (i in 1:n.tokenizer.sentence){
  
  #i = 1 #2,3
  lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_sentence/tokenizer_sentence_%d.R", i), sep = "")
  source(lien)
  tokenizer.sentence.i <- sprintf("tokenizer.sentence.%d(original_books_bis)", i)
  if (DEBUG == TRUE) { print(tokenizer.sentence.i) }
  token_sentence <- eval(parse(text=tokenizer.sentence.i))#[[1]][1]
  if (DEBUG == TRUE) { print(token_sentence) }
  nb.of.sentence[i] <- dim(token_sentence)[1]
  if (DEBUG == TRUE) { print(nb.of.sentence) }
  #token_word <- c()
  
  for (j in 1:n.tokenizer.word.occu){
    
    token_word <- c()
    #j = 3 #3,4,5
    if(i==1) {
      lien <- paste(my_path,sprintf("/Intership_NLP_CU/preprocessing/tokenizer_word_occu/tokenizer_word_occu_%d.R", j), sep="")
      source(lien)
    }
    tokenizer.word.i <- sprintf("tokenizer.word.%d(token_sentence[k,],k)", j)
    if (DEBUG == TRUE) { print(tokenizer.word.i) }
    
    for(k in 1:nb.of.sentence[i]) {
      #k = 1 #2,3,4...15773
      if (DEBUG == TRUE) { token_sentence[k,] }
      new_token_word <- eval(parse(text=tokenizer.word.i))
      if (DEBUG == TRUE) { new_token_word }
      token_word <- dplyr::bind_rows(token_word,new_token_word) #TODO mettre bout à bout des matrice
      if (DEBUG == TRUE) { token_word }
      #nb.of.word.occu[i]  <- sum(token_word[2])
      #nb.of.word.type[i]  <- dim(token_word[2])[1] 
    }
    ###TO DO NOMBREs de mots a verifier 464,193, 464,194
    
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
    
  }
  
}

####################################   Doing the choices for the check boxes  ##########################################################
book_unique <- unique(original_books_bis$book)
check_choices <- c()
check_choices_token_sentence_check <- c()
check_choices_token_word_check <- c()
for(i in book_unique){
  a_paste_local <- paste("Book", i, sep = "" )
  check_choices <- c(check_choices, a_paste_local)
}
count_1 = 1
while(count_1 <= n.tokenizer.sentence){
  b_paste_local <- paste("TokenizationSentence", count_1, sep = "" )
  check_choices_token_sentence_check <- c(check_choices_token_sentence_check, b_paste_local)
  count_1 =count_1 +1
}
count_2 = 1
while(count_2 <= n.tokenizer.word.occu){
  c_paste_local <- paste("TokenizationWord", count_2, sep = "" )
  check_choices_token_word_check <- c(check_choices_token_word_check, c_paste_local)
  count_2 =count_2 +1
}

m <- 400