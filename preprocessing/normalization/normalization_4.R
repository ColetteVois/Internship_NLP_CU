#' @description Normalize the words of the text. Using "text_tokens()" function from "corpus" package with  stemmer = stem_list argument.
#' 
#' @param token_word_freq A tibble with four colums. 
#' token_sentence$word are the words of the text in alphabetical order occuring just one. 
#' token_sentence$sentences is the list of numbers of sentences (line of the sentence in token_sentence) in which each word appear.
#' token_sentence$freq is the frequence each word appears in the text.
#' token_sentence$tf is the terme frequency of each word.
#' @return token_word_stem A tibble with four colums. 
#' token_word_stem$word are the normalize form of words of the text in alphabetical order occuring just ones. 
#' token_word_stem$sentences is the list of numbers of sentences (line of the sentence in token_sentence) in which each normalize word appear.
#' token_word_stem$freq is the frequence each normalize word appears in the text.
#' 
#' @import corpus
#' @examples
#' ## library(corpus)
#' ## token_word_stem <- normalize.4(token_word_freq)

# download the list
url <- "http://www.lexiconista.com/Datasets/lemmatization-en.zip"
tmp <- tempfile()
download.file(url, tmp)

# extract the contents
con <- unz(tmp, "lemmatization-en.txt", encoding = "UTF-8")
tab <- read.delim(con, header=FALSE, stringsAsFactors = FALSE)
names(tab) <- c("stem", "term")

head(tab)

stem_list <- function(term) {
  i <- match(term, tab$term)
  if (is.na(i)) {
    stem <- term
  } else {
    stem <- tab$stem[[i]]
  }
  return(stem)
}

normalize.4 <- function(my.texte) {
  
  # my.texte <- token_word_freq
  
  my_texte <- my.texte[1]
  names(my_texte) <- "text"
  
  tokens1 <- text_tokens(my_texte, stemmer = stem_list)
  tokens2 <- unlist(tokens1, recursive=FALSE)
  tokens3 <- sort(tokens2)
  
  listfiles <- unique(tokens3)
  pre_curseur <- 1
  curseur <- 1
  col_word <- c()
  col_sentence <- c()
  col_freq <- c()
  for(word in 1:length(listfiles)) {
    #word = 13
    freq <- 0
    sentence <- c()
    while(identical(listfiles[word], tokens3[curseur])) {
      freq <- freq + my.texte[curseur,]$freq
      sentence <- c(sentence, unlist(my.texte[curseur,]$sentences))
      curseur <- curseur + 1
    }
    col_word <- c(col_word, tokens3[(curseur-1)])
    col_sentence <- c(col_sentence, list(sentence))
    col_freq <- c(col_freq,freq)
    pre_curseur <- curseur
  }
  
  token_word_stem <- tibble(word = col_word, sentences = col_sentence, freq = col_freq)
  
  return(token_word_stem)
}

# token_word_stem <- normalize.4(token_word_freq)