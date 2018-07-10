library(tibble)
library(tidytext)

################################ USER CHANGE ############################################

#write your path to go to your file
my_path <- "C:/Users/rubik/Desktop/Intership_NLP_CU"
#choose which data you want to load
choose_load_data <- 3
#choose your tokenizer 
choose_tokenizer_word <- 1
DEBUG <- TRUE

################################ LOAD DATA ##############################################

source(paste(my_path, sprintf("/Intership_NLP_CU/load_data/load_data_%d.R", choose_load_data), sep = ""))
load.data.i <- sprintf("load.data.%d()", choose_load_data)
original_books <- eval(parse(text=load.data.i))
book.length <- nrow(original_books)

################################# TOKENIZER SENTENCE ###################################

lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_word_occu/tokenizer_word_occu_%d.R", choose_tokenizer_word), sep="")
source(lien)

original_books2 <- as_tibble(original_books)
tokenizer.word.i <- sprintf("tokenizer.word.%d(original_books2)", choose_tokenizer_word)

token_word <- eval(parse(text=tokenizer.word.i))

weight_token_word <- token_word %>% bind_tf_idf(token_word$word,token_word$n)

