library(dplyr)
library(janeaustenr)
library(stringr)
library(tidytext)

DEBUG = TRUE



################################# MAIN 1 AUSTEN #######################################
#get the example
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

if (DEBUG == TRUE) {original_books}

###################################

article1 <- read.csv("C:/Users/rubik/Desktop/Intership_NLP_CU/Document/craft-2.0/articles/txt/11532192.txt", sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)
article2 <- read.csv("C:/Users/rubik/Desktop/Intership_NLP_CU/Document/craft-2.0/articles/txt/11597317.txt", sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)
article3 <- read.csv("C:/Users/rubik/Desktop/Intership_NLP_CU/Document/craft-2.0/articles/txt/11897010.txt", sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)
article4 <- read.csv("C:/Users/rubik/Desktop/Intership_NLP_CU/Document/craft-2.0/articles/txt/12079497.txt", sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)

article <- merge(article1,article2,all=TRUE)
article <- merge(article,article3,all=TRUE)
article <- merge(article,article4,all=TRUE)

original_books <- as_data_frame(article)

if (DEBUG == TRUE) {original_books}

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
