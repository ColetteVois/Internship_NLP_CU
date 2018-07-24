################################ USER CHANGE ############################################

#write your path to go to your file
#my_path <- "C:/Users/Projet/Intership_NLP_CU"
my_path <- "C:/Users/Projet"
#choose which data you want to load
choose_load_data <- 1
DEBUG = TRUE

################################### LOAD DATA ###########################################

source(paste(my_path, sprintf("/Intership_NLP_CU/load_data/load_data_%d.R", choose_load_data), sep = ""))
load.data.i <- sprintf("load.data.%d()", choose_load_data)
original_books <- eval(parse(text=load.data.i))
original_books <- original_books %>% mutate(rowname = 1:nrow(original_books))
original_books_bis <- original_books[1:1000,]
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


####################################   Doing the choices for the check boxes  ##########################################################
n.tokenizer.sentence <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_sentence/", sep = "")))
n.tokenizer.word.occu <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_word_occu/", sep=""))) - 1
n.normalization <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/normalization/", sep=""))) - 1

book_unique <- unique(original_books_bis$book)
check_choices <- c()
check_choices_token_sentence_check <- c()
check_choices_token_word_check <- c()
check_choices_token_norma_check <- c()
for(i in book_unique){
  a_paste_local <- paste("Book", i, sep = "" )
  check_choices <- c(check_choices, a_paste_local)
}
count_1 = 1
while(count_1 <= n.tokenizer.sentence){
  b_paste_local <- toString(count_1)
  check_choices_token_sentence_check <- c(check_choices_token_sentence_check, b_paste_local)
  count_1 =count_1 +1
}
count_2 = 1
while(count_2 <= n.tokenizer.word.occu){
  c_paste_local <- toString(count_2)
  check_choices_token_word_check <- c(check_choices_token_word_check, c_paste_local)
  count_2 =count_2 +1
}
count_3 = 1
while(count_3 <= n.normalization){
  d_paste_local <- toString(count_3)
  check_choices_token_norma_check <- c(check_choices_token_norma_check, d_paste_local)
  count_3 =count_3 +1
}

m <- 400
