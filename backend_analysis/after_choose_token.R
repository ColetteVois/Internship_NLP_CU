#' @description After having choosen the different tokenisations doing all the analysis of the text.
#' 
#' @param original_books_bis A tibble with two columns. 
#' original_book$text is the lines of the text which has been selected. 
#' original_book$book is the part (e.g. chapter, different book...) 
#' of the full text to which the lines belong
#' @param choose_tokenizer_sentence A integer, the number of the token_sentence function
#' @param choose_tokenizer_word A integer, the number of the token_word function
#' @param choose_normalization A integer, the number of the token_normalization function
#' 
#' @return token_sentence A tibble with two columns. 
#' token_sentence$sentence is each sentence of the text. 
#' token_sentence$book is the part (e.g. chapter, different book...) 
#' of the full text to which the sentence belong
#' @return token_word A tibble with two columns.
#' token_word$word is each word of the text, in the same order as in the text
#' token_word$sentence is the number of the sentence each word belongs
#' token_word$book is the name of the book each word belongs
#' @return token_word_freq A tibble with four colums. 
#' token_sentence$word are the words of the text in alphabetical order occuring just one. 
#' token_sentence$sentences is the list of numbers of sentences (line of the sentence in token_sentence) in which each word appear.
#' token_sentence$freq is the frequence each word appears in the text.
#' token_sentence$tf is the terme frequency of each word.
#' @return token_word_stem A tibble with four colums. 
#' token_word_stem$word are the normalize form of words of the text in alphabetical order occuring just ones. 
#' token_word_stem$sentences is the list of numbers of sentences (line of the sentence in token_sentence) in which each normalize word appear.
#' token_word_stem$freq is the frequence each normalize word appears in the text.
#' @return token_word_stop A tibble with three columns.
#' token_word_stop$word are the normalize form of words of the text in alphabetical order occuring just ones minus the most current words.
#' token_word_stop$sentences  is the list of numbers of sentences (line of the sentence in token_sentence) in which each word appear.
#' token_word_stop$freq is the frequence each word appears in the text.
#' 
#' @import stringi
#' @examples
#' ## library(stringi)
#' ## token_info <- after.choose.token(original_books_bis, 1, 1, 1) 
#' ## token_sentence <- token_info[[1]]
#' ## token_word <- token_info[[2]]
#' ## token_word_freq <- token_info[[3]]
#' ## token_word_stem <- token_info[[4]]
#' ## token_word_stop <- token_info[[5]]

after.choose.token <- function(my.texte, choose_tokenizer_sentence, choose_tokenizer_word, choose_normalization) {
  
  # my.texte <- original_books_bis
  # choose_tokenizer_sentence <- 1
  # choose_tokenizer_word <- 1
  # choose_normalization <- 1
  
  #if there is no text
  if(nrow(my.texte)==0) {
    return(c(list(tribble(~sentence,~book)), list(tribble(~word,~sentence,~book)), list(tribble(~word,~sentences,~freq))))
  }

  token_word <- c()
  
  lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_word/tokenizer_word_%d.R", choose_tokenizer_word), sep="")
  source(lien)
  
  lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_sentence/tokenizer_sentence_%d.R", choose_tokenizer_sentence), sep = "")
  source(lien)
  
  lien <- paste(my_path,sprintf("/Intership_NLP_CU/preprocessing/normalization/normalization_%d.R", choose_normalization), sep="")
  source(lien)
  
  lien <- paste(my_path,sprintf("/Intership_NLP_CU/preprocessing/stop_word/stop_word_%d.R", choose_normalization), sep="")
  source(lien)

  tokenizer.sentence.i <- sprintf("tokenizer.sentence.%d(my.texte)", choose_tokenizer_sentence)
  token_sentence <- eval(parse(text=tokenizer.sentence.i))
    
  tokenizer.word.i <- sprintf("tokenizer.word.%d(token_sentence[k,],k)", choose_tokenizer_word)
    
  for(k in 1:dim(token_sentence)[1]) {
    #k = 2
    new_token_word <- eval(parse(text=tokenizer.word.i))
    token_word <- dplyr::bind_rows(token_word,new_token_word) 
  }
  
  token_word_sort <- token_word %>% arrange(word)
  pre_curseur <- 1
  curseur <- 1
  token_word_freq <- c()
  while(curseur < nrow(token_word_sort)) {
    while(identical(token_word_sort$word[pre_curseur], token_word_sort$word[curseur])) {
      curseur <- curseur + 1
    }
    list_sentence <- unique(token_word_sort[[2]][pre_curseur:(curseur - 1)])
    freq <- curseur - pre_curseur
    token_word_freq1 <- list(word = token_word_sort[[1]][pre_curseur], sentences = list(list_sentence), freq = freq)
    names(token_word_freq1) <- c("word","sentences", "freq")
    token_word_freq <- dplyr::bind_rows(token_word_freq, as_tibble(token_word_freq1))
    pre_curseur <- curseur
  }
  
  
  normalization.i <- sprintf("normalize.%d(token_word_freq)", choose_normalization)
  token_word_stem <- eval(parse(text=normalization.i))
  
  token_word_stop <- stop.word.1(token_word_freq)
  token_word_stem_stop <- stop.word.1(token_word_stem)
  
  # tf
  token_word_freq <- token_word_freq %>% mutate(book = "all")
  token_word_freq <- token_word_freq %>% bind_tf_idf(word, book, freq) 
  token_word_stop <- token_word_stop %>% mutate(book = "all")
  token_word_stop <- token_word_stop %>% bind_tf_idf(word, book, freq) 
  token_word_stem_stop <- token_word_stem_stop %>% mutate(book = "all")
  token_word_stem_stop <- token_word_stem_stop %>% bind_tf_idf(word, book, freq) 

  # take off useless columns
  token_word_freq <- token_word_freq[ , - c(4,6,7)]
  
  return(c(list(token_sentence), list(token_word), list(token_word_freq), list(token_word_stem), list(token_word_stop), list(token_word_stem_stop)))
}

# token_info <- after.choose.token(original_books_bis, 1, 1, 1)
# 
# token_sentence <- token_info[[1]]
# token_word <- token_info[[2]]
# token_word_freq <- token_info[[3]]
# token_word_stem <- token_info[[4]]
# token_word_stop <- token_info[[5]]

