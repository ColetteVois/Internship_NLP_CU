library(tm)
library(NLP)
library(openNLP)
library(tibble)


tokenizer.sentence.2.bis <- function(text, lang = "en") {
  #text = as_tibble(original_books[[1]][pre_curseur:curseur])
  #lang = "en"
  
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  
  # Convert text to class String from package NLP
  text <- as.String(text)
  
  # Sentence boundaries in text
  sentence.boundaries <- NLP::annotate(text, sentence_token_annotator)
  
  # Extract sentences
  sentences1 <- text[sentence.boundaries]
  sentences <- list(sentences1)
  names(sentences) <- "sentence"
  sentences <- as_tibble(sentences)
  names(sentences) <- "sentence"
  
  #nb.of.words <- length(sentences)
  #print(nb.of.words)
  
  # return sentences
  return(sentences)

}

tokenizer.sentence.2 <- function(my.texte) {
  
  #my.texte <- original_books_bis
  
  listfiles <- unique(my.texte[[2]])
  pre_curseur <- 1
  curseur <- 1
  tokens <- c()
  col_2 <- c()
  for(docu in 1:length(listfiles)) {
    #docu = 2
    while(identical(listfiles[docu], my.texte[[2]][curseur])) {
      curseur <- curseur + 1
    }
    new_token <- tokenizer.sentence.2.bis(as_tibble(original_books[[1]][pre_curseur:curseur]))[[1]]
    tokens <- c(tokens, new_token)
    col_2 <- c(col_2, rep(toString(listfiles[docu]), length(new_token)))
    pre_curseur <- curseur
    
  }
 
  tokens <- as_tibble(tokens)
  tokens <- tokens %>% mutate(book = col_2)
  names(tokens) <- c("sentence","book")
  
  return(tokens)
}

#tokenizer.sentence.2(original_books)
