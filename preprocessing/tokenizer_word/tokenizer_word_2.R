#' @description Divides the text into its different words. Using "stringr" package.
#' 
#' @param token_sentence[k,] A tibble with two columns and just one row. 
#' token_sentence$sentence is the k th sentence of the text. 
#' token_sentence$book is the part (e.g. chapter, different book...) 
#' of the full text to which this sentence belongs.
#' @param k A integer, the number of the sentence in the text
#' @return token_word A tibble with two columns
#' token_word$word is each word of the sentence k, in the same order as in the sentence
#' token_word$sentence is the number of the sentence each wod belongs
#' token_word$book is the name of the book each word belongs
#' 
#' @import stringr (install.packages("stringr", dependencies = TRUE)), dplyr
#' @examples
#' ## library(stringr)
#' ## library(dplyr)
#' ## token_word <- tokenizer.word.2(token_sentence[k,], k)

Clean_String <- function(string){
  
  # Lowercase
  temp <- tolower(string)
  # Remove everything that is not a number or letter (may want to keep more 
  # stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  
  return(temp)
}

# function to clean text
Clean_Text_Block <- function(text){
  
  # Get rid of blank lines
  indexes <- which(text == "")
  if (length(indexes) > 0) {
    text <- text[-indexes]
  }
  # See if we are left with any valid text:
  if (length(text) == 0) {
    cat("There was no text in this document! \n")
    to_return <- list(num_tokens = 0, 
                      unique_tokens = 0, 
                      text = "")
  } else {
    # If there is valid text, process it.
    # Loop through the lines in the text and combine them:
    clean_text <- NULL
    for (i in 1:length(text)) {
      # add them to a vector 
      clean_text <- c(clean_text, Clean_String(text[i]))
    }
    # Calculate the number of tokens and unique tokens and return them in a 
    # named list object.
    num_tok <- length(clean_text)
    num_uniq <- length(unique(clean_text))
    to_return <- list(num_tokens = num_tok, 
                      unique_tokens = num_uniq, 
                      text = clean_text)
  }
  
  return(to_return)
}

tokenizer.word.2 <- function(my.texte,k) {
  
  #k = 264
  #my.texte <- token_sentence[k,]
  
  book_name <- my.texte[2][[1]]
  clean_speech <- Clean_Text_Block(my.texte[[1]])
  tokens <- as_tibble(clean_speech$text)
  names(tokens) <- "word"
  token_word <- tokens %>% mutate(sentence = k, book = book_name)
  
  return(token_word)
}

#token_word <- tokenizer.word.2(token_sentence[k,], k)
