#' @description Applied Heaps Law to the text.
#' 
#' @param original_books_bis A tibble with two columns. 
#' original_book$text is the lines of the text which has been selected. 
#' original_book$book is the part (e.g. chapter, different book...) 
#' of the full text to which the lines belong
#' @param choose_tokenizer_sentence A integer, the number of the token_sentence function
#' @param choose_tokenizer_word A integer, the number of the token_word function
#' @return nb.of.word.occu A list of integer, number of words occurences for differents parts of the text
#' @return nb.of.stop.word A list of integer, number of words with stopword applied for differents parts of the text
#' 
#' @import tibble
#' @examples
#' ## library(tibble)
#' ## heaps.law(original_books_bis, 1, 1)

heaps.law <- function(my.texte, choose_tokenizer_sentence, choose_tokenizer_word) {

  #my.texte <- original_books[1:400,]
  #choose_tokenizer_sentence <- 1
  #choose_tokenizer_word <- 1
  
  book.length <- nrow(my.texte)
    
  lien <- paste(my_path, sprintf("/Intership_NLP_CU-master/preprocessing/tokenizer_word/tokenizer_word_%d.R", choose_tokenizer_word), sep="")
  source(lien)
  
  lien <- paste(my_path, "/Intership_NLP_CU-master/preprocessing/stop_word/stop_word_1.R", sep="")
  source(lien)
  
  lien <- paste(my_path, sprintf("/Intership_NLP_CU-master/preprocessing/tokenizer_sentence/tokenizer_sentence_%d.R", choose_tokenizer_sentence), sep = "")
  source(lien)
  
  nb.of.word.occu <- c()
  nb.of.stop.word <- c()
  
  # for changing the number of point you want
  accura <- 2
  
  withProgress( expr ={
    for(i in 1:(10^accura)) {
      #i <- 2
      token_word <- c()
    
      extrait.length <- round(i*(0.1^accura)*book.length)
      original_books2 <- as_tibble(my.texte[1:extrait.length,1:2])
    
      tokenizer.sentence.i <- sprintf("tokenizer.sentence.%d(original_books2)", choose_tokenizer_sentence)
      token_sentence <- eval(parse(text=tokenizer.sentence.i))#[[1]][1]

      tokenizer.word.i <- sprintf("tokenizer.word.%d(token_sentence[k,],k)", choose_tokenizer_word)
    
      for(k in 1:dim(token_sentence)[1]) {
        #k = 1
        new_token_word <- eval(parse(text=tokenizer.word.i))
        token_word <- dplyr::bind_rows(token_word,new_token_word) 
        inc <- 1/(dim(token_sentence)[1]* 1:(10^accura))
        incProgress(amount = inc)
      }
    
    nb.of.word.occu[i] <- nrow(token_word)
    
    stop_word <- stop.word.1(token_word)
    nb.of.stop.word[i] <- nrow(stop_word)
  }
  },
message = "Creating plots"
  )
  
  return(c(list(nb.of.word.occu), list(nb.of.stop.word)))
}