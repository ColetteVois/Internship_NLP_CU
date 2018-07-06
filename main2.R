################################ USER CHANGE ############################################

#write your path to go to your file
my_path <- "C:/Users/rubik/Desktop/Intership_NLP_CU"
#choose which data you want to load
choose_load_data <- 1
#choose your tokenizer 
choose_tokenizer_sentence <- 1
DEBUG <- TRUE

################################ LOAD DATA ##############################################

source(paste(my_path, sprintf("/Intership_NLP_CU/load_data/load_data_%d.R", choose_load_data), sep = ""))
load.data.i <- sprintf("load.data.%d()", choose_load_data)
original_books <- eval(parse(text=load.data.i))


################################# TOKENIZER SENTENCE ###################################

lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_word_occu/tokenizer_word_occu_%d.R", choose_tokenizer_sentence), sep="")
source(lien)
tokenizer.word.i <- sprintf("tokenizer.word.%d(original_books)", choose_tokenizer_sentence)
if (DEBUG == TRUE) { print(tokenizer.word.i) }
token_word <- eval(parse(text=tokenizer.word.i))

nb.of.words <- sum(token_word[2])
nb.of.types <- dim(token_word[2])[1]


#names(token_sentence_infos[2])[1] <- "sentence"
#token_word <- token_word_infos[2]
#token_word <- as_tibble(token_word)
#tidy_books_count = token_word %>%
#  count(word, sort = TRUE) 
#nb.of.word <- token_word_infos[1]