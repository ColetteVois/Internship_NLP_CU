library(tm)
library(NLP)
library(openNLP)
library(tibble)


tokenizer.sentence.2 <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  
  # Convert text to class String from package NLP
  text <- as.String(original_books)
  
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  # Extract sentences
  sentences <- text[sentence.boundaries]
  names(sentences) <- "sentence"
  sentences <- as_tibble(sentences)
  names(sentences) <- "sentence"
  
  #nb.of.words <- length(sentences)
  #print(nb.of.words)
  
  # return sentences
  return(sentences)
  #30844
}

#tokenizer.sentence.2(original_books)