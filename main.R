################################ LOAD DATA ##############################################

source("C:/Users/rubik/Desktop/Intership_NLP_CU/Intership_NLP_CU/load_data/load_data_3.R")
 
original_books <- load.data.3()


################################# TOKENIZER SENTENCE ###################################

n.tokenizer.sentence <- length(list.files("C:/Users/rubik/Desktop/Intership_NLP_CU/Intership_NLP_CU/preprocessing/tokenizer_sentence/"))

nb.of.sentence <- c()

for (i in 1:n.tokenizer.sentence){
  lien <- sprintf("C:/Users/rubik/Desktop/Intership_NLP_CU/Intership_NLP_CU/preprocessing/tokenizer_sentence/tokenizer_sentence_%d.R", i)
  source(lien)
  tokenizer.sentence.i <- sprintf("tokenizer.sentence.%d(original_books)", i)
  print(tokenizer.sentence.i)
  nb.of.sentence[i] <- eval(parse(text=tokenizer.sentence.i))
}
nb.of.sentence

boxplot(nb.of.sentence, main = "Sentence", asp = 1)

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
