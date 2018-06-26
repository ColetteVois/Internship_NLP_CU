library(dplyr)
library(janeaustenr)
library(stringr)
library(tidytext)

DEBUG = TRUE

#---------------------------------MAIN-----------------------------------#
#get the example
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

if (DEBUG == TRUE) {original_books}

#------------------------------------WORD------------------------------------#

#############################TOKEN 1

tokenizer.word.1 <- function(my.texte) {
  tidy_books <- my.texte %>%
    unnest_tokens(word, text)
  if (DEBUG == TRUE) {tidy_books}
  tidy_books_count = tidy_books %>%
    count(word, sort = TRUE) 
  if (DEBUG == TRUE) {tidy_books_count}
  nb.of.words <- sum(tidy_books_count[2])
  return(nb.of.words)
  #725055
}

tokenizer.word.1(original_books)

###############################TOKEN 2

tokenizer.word.2 <- function(my.texte) {
  tidy_books <- my.texte %>%
    unnest_tokens(word, text)
  cleaned_books <- tidy_books %>%
    anti_join(get_stopwords())
  if (DEBUG == TRUE) {cleaned_books} 
  cleaned_books_count = cleaned_books %>%
    count(word, sort = TRUE)
  if (DEBUG == TRUE) {cleaned_books_count} 
  nb.of.words <- sum(cleaned_books_count[2])
  return(nb.of.words)
  # 325084
}

tokenizer.word.2(original_books)



###############################TOKEN 3
library("tm")

tokenizer.word.3 <- function(my.texte) {
  tokens <- Boost_tokenizer(my.texte[1])
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- length(tokens)
  return(nb.of.words)
  #728907
}

tokenizer.word.3(original_books)


###############################TOKEN 4

library(tokenizers)

tokenizer.word.4 <- function(my.texte) {
  tokens <- tokenize_words(paste0(my.texte[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- dim(as.data.frame(tokens))
  return(nb.of.words)
  #725056
}

tokenizer.word.4(original_books)

##############################TOKEN 5

library(tokenizers)

tokenizer.word.5 <- function(my.texte) {
  tokens <- tokenize_tweets(paste0(my.texte[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- dim(as.data.frame(tokens))
  return(nb.of.words)
  #717497
}

tokenizer.word.5(original_books)

############################TOKEN 6


#-----------------------------------------SENTENCE---------------------------#

############################TOKEN 1

library(tokenizers)

tokenizer.sentence.1 <- function(my.texte) {
  tokens <- tokenize_sentences(paste0(my.texte[1]), lowercase = TRUE)
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- dim(as.data.frame(tokens))
  return(nb.of.words)
  #31396
}

tokenizer.sentence.1(original_books)

#------------------------------------NOMALISATION----------------------------#
#token by hand
#lowercase, see options
#leon
#######################################################################


