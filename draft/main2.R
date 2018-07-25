library(tibble)

################################ USER CHANGE ############################################

#write your path to go to your file
my_path <- "C:/Users/rubik/Desktop/Intership_NLP_CU"
#choose which data you want to load
choose_load_data <- 1
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

lien <- paste(my_path, "/Intership_NLP_CU/preprocessing/stop_word/stop_word_1.R", sep="")
source(lien)

nb.of.word.occu <- c()
nb.of.stop.word <- c()

for(i in 1:100)
{
  extrait.length <- round(i*0.01*book.length)
  original_books2 <- as_tibble(original_books[1:extrait.length,1])
  tokenizer.word.i <- sprintf("tokenizer.word.%d(original_books2)", choose_tokenizer_word)
  if (DEBUG == TRUE) { print(tokenizer.word.i) }
  token_word <- eval(parse(text=tokenizer.word.i))
  nb.of.word.occu[i] <- sum(token_word[2])
  
  stop_word <- stop.word.1(token_word)
  nb.of.stop.word[i] <- nrow(stop_word)
  
}

jpeg(paste(my_path, sprintf('/Intership_NLP_CU/boxplot/log_heaps_law_data_%d.jpg',choose_load_data),sep =""))
plot(log(nb.of.word.occu),log(nb.of.stop.word))
reg_lin <- lm(log(nb.of.stop.word) ~ log(nb.of.word.occu))
abline(reg_lin)
dev.off()

K <- exp(reg_lin$coefficients[[1]]) #normalement entre 30 et 100
beta <- reg_lin$coefficients[[2]]   #environ 0.5
summary(reg_lin)

jpeg(paste(my_path, sprintf('/Intership_NLP_CU/boxplot/heaps_law_data_%d.jpg',choose_load_data),sep =""))
plot(nb.of.word.occu, nb.of.stop.word)
lines(nb.of.word.occu, K*nb.of.word.occu^beta, col="red")
dev.off()

