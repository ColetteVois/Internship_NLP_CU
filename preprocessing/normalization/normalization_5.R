###########################NORMALIZE 5

#attention ici avec meme tokenization de base pour tweet


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
  stem
}

normalize.5 <- function(my.texte) {
  if(tweet == TRUE)#TO DO a mieux faire
  {
    tokens0 <- my.texte %>%
      unnest_tokens(text, text)
    if (DEBUG == TRUE) {tokens0} 
  } else {
    tokens0 <- my.texte
  }
  tokens1 <- text_tokens(tokens0, stemmer = stem_list)
  tokens2 <- unlist(tokens1, recursive=FALSE)
  tokens <- unique(tokens2)
  if (DEBUG == TRUE) {tokens} 
  nb.of.words <- length(tokens)
  return(nb.of.words)
  #725056
}

normalize.5(original_books)
