library("quanteda")

tokenizer.sentence.3 <- function(my.texte) {
  token <- unlist(my.texte, recursive=FALSE)
  sentence <- tokens(token, what = "sentence", remove_numbers = FALSE, remove_punct = FALSE,
                     remove_symbols = FALSE, remove_separators = TRUE,
                     remove_twitter = FALSE, remove_hyphens = FALSE, remove_url = FALSE,
                     ngrams = 1L, skip = 0L, concatenator = "_",
                     verbose = quanteda_options("verbose"), include_docvars = TRUE)
  nb.of.words <-   length(sentence)
  print(nb.of.words)
  return(nb.of.words)
  #31396
}

#tokenizer.sentence.3(original_books[1]) #pour austen
#pour article