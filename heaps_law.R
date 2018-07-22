library(tibble)

heaps.law <- function(my.texte, choose_tokenizer_sentence, choose_tokenizer_word) {

  #my.texte <- original_books[1:400,]
  #choose_tokenizer_sentence <- 2
  #choose_tokenizer_word <- 1
  
  book.length <- nrow(my.texte)
    
  lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_word_occu/tokenizer_word_occu_%d.R", choose_tokenizer_word), sep="")
  source(lien)
  
  lien <- paste(my_path, "/Intership_NLP_CU/preprocessing/stop_word/stop_word_1.R", sep="")
  source(lien)
  
  lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_sentence/tokenizer_sentence_%d.R", choose_tokenizer_sentence), sep = "")
  source(lien)
  
  nb.of.word.occu <- c()
  nb.of.stop.word <- c()
  
  accura <- 2
  
  for(i in 1:(10^accura)) {
    #i <- 2
    
    token_word <- c()
    
    extrait.length <- round(i*(0.1^accura)*book.length)
    original_books2 <- as_tibble(my.texte[1:extrait.length,1:2])
    
    tokenizer.sentence.i <- sprintf("tokenizer.sentence.%d(original_books2)", choose_tokenizer_sentence)
    token_sentence <- eval(parse(text=tokenizer.sentence.i))#[[1]][1]

    tokenizer.word.i <- sprintf("tokenizer.word.%d(token_sentence[k,])", choose_tokenizer_word)
    
    for(k in 1:dim(token_sentence)[1]) {
      #k = 1
      new_token_word <- eval(parse(text=tokenizer.word.i))
      token_word <- dplyr::bind_rows(token_word,new_token_word) 
    }
      
    nb.of.word.occu[i] <- nrow(token_word)
    
    stop_word <- stop.word.1(token_word)
    nb.of.stop.word[i] <- nrow(stop_word)
    
  }
  
  return(c(list(nb.of.word.occu), list(nb.of.stop.word)))
}


a = heaps.law(original_books_bis, 1, 1)
nb.of.word.occu = a[[1]]
nb.of.stop.word = a[[2]]

#24, 25,26
#jpeg(paste(my_path, sprintf('/Intership_NLP_CU/boxplot/log_heaps_law_data_%d.jpg',choose_load_data),sep =""))
plot(log(nb.of.word.occu),log(nb.of.stop.word))
reg_lin <- lm(log(nb.of.stop.word) ~ log(nb.of.word.occu))
abline(reg_lin)
#dev.off()

K <- exp(reg_lin$coefficients[[1]]) #normalement entre 30 et 100
beta <- reg_lin$coefficients[[2]]   #environ 0.5
summary(reg_lin)

#jpeg(paste(my_path, sprintf('/Intership_NLP_CU/boxplot/heaps_law_data_%d.jpg',choose_load_data),sep =""))
plot(nb.of.word.occu, nb.of.stop.word)
lines(nb.of.word.occu, K*nb.of.word.occu^beta, col="red")
#dev.off()