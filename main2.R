################################ USER CHANGE ############################################

#write your path to go to your file
my_path <- "C:/Users/rubik/Desktop/Intership_NLP_CU"
#choose which data you want to load
choose_load_data <- 1
#choose your tokenizer 
choose_tokenizer_sentence <- 1
DEBUG = TRUE

################################ LOAD DATA ##############################################

source(paste(my_path, sprintf("/Intership_NLP_CU/load_data/load_data_%d.R", choose_load_data), sep = ""))
load.data.i <- sprintf("load.data.%d()", choose_load_data)
original_books <- eval(parse(text=load.data.i))


################################# TOKENIZER SENTENCE ###################################

lien <- sprintf("C:/Users/rubik/Desktop/Intership_NLP_CU/Intership_NLP_CU/preprocessing/tokenizer_sentence/tokenizer_sentence_%d.R", choose_tokenizer_sentence)
source(lien)
tokenizer.sentence.i <- sprintf("tokenizer.sentence.%d(original_books)", choose_tokenizer_sentence)
if (DEBUG == TRUE) { print(tokenizer.sentence.i) }
token_sentence_infos <- eval(parse(text=tokenizer.sentence.i))
#names(token_sentence_infos[2])[1] <- "sentence"
token_sentence <- list(sentence = token_sentence_infos[2][[1]])
nb.of.sentence <- token_sentence_infos[1]
mutate(original_books, sentences = token_sentence)

if (DEBUG == TRUE) { print(original_books) }

################################# TOKENIZER WORD ###################################

n.tokenizer.word.occu <- length(list.files("C:/Users/rubik/Desktop/Intership_NLP_CU/Intership_NLP_CU/preprocessing/tokenizer_word_occu/")) - 1

nb.of.word.occu <- c()

for (i in 1:n.tokenizer.word.occu){
  lien <- sprintf("C:/Users/rubik/Desktop/Intership_NLP_CU/Intership_NLP_CU/preprocessing/tokenizer_word_occu/tokenizer_word_occu_%d.R", i)
  source(lien)
  tokenizer.word.i <- sprintf("tokenizer.word.%d(original_books)", i)
  print(tokenizer.word.i)
  nb.of.word.occu[i] <- eval(parse(text=tokenizer.word.i))
}
nb.of.word.occu

boxplot(nb.of.word.occu, main = "Word Occurence", asp = 1)

################################# NORMALIZATION ###################################

n.normalization <- length(list.files("C:/Users/rubik/Desktop/Intership_NLP_CU/Intership_NLP_CU/preprocessing/normalization/")) 

nb.of.normalization <- c()

for (i in 1:n.normalization){
  lien <- sprintf("C:/Users/rubik/Desktop/Intership_NLP_CU/Intership_NLP_CU/preprocessing/normalization/normalization_%d.R", i)
  source(lien)
  normalization.i <- sprintf("normalize.%d(original_books)", i) #TODO normalization
  print(normalization.i)
  nb.of.normalization[i] <- eval(parse(text=normalization.i))
}
nb.of.normalization

boxplot(nb.of.normalization, main = "Normalization", asp = 1)
