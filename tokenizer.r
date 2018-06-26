library(dplyr)
library(janeaustenr)
library(stringr)
library(tidytext)

DEBUG = TRUE

#get the example
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

if (DEBUG == TRUE) {original_books}

##############################################TOKEN 1

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

###########################################TOKEN 2

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



##################################################DRAFT




library("tm")

tokens <- Boost_tokenizer(austen_books()[1])
#728907

library(tokenizers)

token3 <- tokenize_words(paste0(austen_books()[1]), lowercase = TRUE)
length(token3[1,1])
#725056

#######################################################################

nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  semi_join(nrcjoy) %>%
  count(word, sort = TRUE)

library(tidyr)
bing <- get_sentiments("bing")

janeaustensentiment <- tidy_books %>%
  inner_join(bing) %>%
  count(book, index = line %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

