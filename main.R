################################ USER CHANGE ############################################

#write your path to go to your file
my_path <- "C:/Users/rubik/Desktop/Intership_NLP_CU"
#choose which data you want to load
choose_load_data <- 3
DEBUG = TRUE

################################### LOAD DATA ###########################################

source(paste(my_path, sprintf("/Intership_NLP_CU/load_data/load_data_%d.R", choose_load_data), sep = ""))
load.data.i <- sprintf("load.data.%d()", choose_load_data)
original_books <- eval(parse(text=load.data.i))

################################# TOKENIZER SENTENCE ###################################

n.tokenizer.sentence <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_sentence/", sep = "")))

nb.of.sentence <- c()

for (i in 1:n.tokenizer.sentence){
  
  lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_sentence/tokenizer_sentence_%d.R", i), sep = "")
  source(lien)
  tokenizer.sentence.i <- sprintf("tokenizer.sentence.%d(original_books)", i)
  if (DEBUG == TRUE) { print(tokenizer.sentence.i) }
  nb.of.sentence[i] <- eval(parse(text=tokenizer.sentence.i))[[1]][1]
  
}

if (DEBUG == TRUE) {print(nb.of.sentence)}

jpeg(paste(my_path, sprintf('/Intership_NLP_CU/boxplot/Sentence_data_%d.jpg',choose_load_data),sep =""))
boxplot(nb.of.sentence, main = "Sentence", asp = 1)
dev.off()

################################# TOKENIZER WORD ###################################

n.tokenizer.word.occu <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_word_occu/", sep=""))) - 1

nb.of.word.occu <- c()

for (i in 1:n.tokenizer.word.occu){
  
  lien <- paste(my_path,sprintf("/Intership_NLP_CU/preprocessing/tokenizer_word_occu/tokenizer_word_occu_%d.R", i), sep="")
  source(lien)
  tokenizer.word.i <- sprintf("tokenizer.word.%d(original_books)", i)
  print(tokenizer.word.i)
  nb.of.word.occu[i] <- eval(parse(text=tokenizer.word.i))
  
}
if (DEBUG == TRUE) {print(nb.of.word.occu)}

jpeg(paste(my_path, sprintf('/Intership_NLP_CU/boxplot/Word_data_%d.jpg',choose_load_data),sep =""))
boxplot(nb.of.word.occu, main = "Word Occurence", asp = 1)
dev.off()

################################# NORMALIZATION ###################################

n.normalization <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/normalization/", sep=""))) 

nb.of.normalization <- c()

for (i in 1:n.normalization){
  
  lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/normalization/normalization_%d.R", i), sep="")
  source(lien)
  normalization.i <- sprintf("normalize.%d(original_books)", i) #TODO normalization
  print(normalization.i)
  nb.of.normalization[i] <- eval(parse(text=normalization.i))
  
}
if (DEBUG == TRUE) {print(nb.of.normalization)}

jpeg(paste(my_path, sprintf('/Intership_NLP_CU/boxplot/Normalization_data_%d.jpg',choose_load_data),sep =""))
boxplot(nb.of.normalization, main = "Normalization", asp = 1)
dev.off()
