#' @description Divides the text into its different sentences. Using "Maxent_Sent_Token_Annotator()" function from "openNLP" library with language = "en" argument.
#' 
#' @param original_books_bis A tibble with two colums. 
#' original_book$text is the lines of the text. 
#' original_book$book is the part (e.g. chapter, different book...) 
#' of the full text to which the lines belong
#' @return token_sentence A tibble with two colums. 
#' token_sentence$sentence is each sentence of the text. 
#' token_sentence$book is the part (e.g. chapter, different book...) 
#' of the full text to which the sentence belong
#' 
#' @import tm, NLP, openNLP, tibble, dplyr
#' #' @examples
#' ## library(tm)
#' ## library(NLP)
#' ## library(openNLP)
#' ## library(tibble)
#' ## library(dplyr)
#' ## token_sentence <- tokenizer.sentence.2(original_books_bis)


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
  
  # return sentences
  return(sentences)

}

tokenizer.sentence.2 <- function(my.texte) {

  #my.texte <- original_books_bis
  
  listfiles <- unique(my.texte[[2]])
  pre_curseur <- 1
  curseur <- 1
  token_sentence <- c()
  col_2 <- c()
  for(docu in 1:length(listfiles)) {
    #docu = 2
    # print(docu)
    while(identical(listfiles[docu], my.texte[[2]][curseur])) {
      curseur <- curseur + 1
    }
    new_token <- tokenizer.sentence.2.bis(as_tibble(my.texte[[1]][pre_curseur:curseur]))[[1]]
    token_sentence <- c(token_sentence, new_token)
    col_2 <- c(col_2, rep(toString(listfiles[docu]), length(new_token)))
    pre_curseur <- curseur
  }
 
  token_sentence <- as_tibble(token_sentence)
  token_sentence <- token_sentence %>% mutate(book = col_2)
  names(token_sentence) <- c("sentence","book")
  
  return(token_sentence)
}

#tokenizer.sentence.2(original_books_bis)
