library("quanteda")

tokenizer.sentence.3 <- function(my.texte) {
  
  token <- unlist(original_books, recursive=FALSE)
  sentence <- tokens(token, what = "sentence", remove_numbers = FALSE, remove_punct = FALSE,
                     remove_symbols = FALSE, remove_separators = TRUE,
                     remove_twitter = FALSE, remove_hyphens = FALSE, remove_url = FALSE,
                     ngrams = 1L, skip = 0L, concatenator = "_",
                     verbose = quanteda_options("verbose"), include_docvars = TRUE)
  sentences <- as_tibble(unlist(sentence))
  names(sentences) <- "sentence"
  #nb.of.words <-   length(sentence)
  #print(nb.of.words)
  return(sentences)

}

#tokenizer.sentence.3(original_books[1])