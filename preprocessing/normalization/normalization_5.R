library("tm")

normalize.2 <- function(my.texte) {
  
  # Load the data as a corpus
  docs <- Corpus(VectorSource(my.texte[1]))
  if (DEBUG == TRUE) {docs}
  
  inspect(docs)
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  #docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  #docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  docs <- tm_map(docs, stemDocument)
  
  dtm <- TermDocumentMatrix(docs)
  if (DEBUG == TRUE) {dtm}
  
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  nb.of.words <- sum(d[2])
  nb.of.types <- dim(d[2])[1]
  return(c(nb.of.words, nb.of.types))
}

normalize.2(original_books)
