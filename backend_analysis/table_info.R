#' @description Create a table to summarize the significative numbers about tokenizations. 
#'  
#' @param token_info A list of tibble, see the output of after_choose_token.
#' @return table_info A tible with the number of documents, occurences of words, type of words...
#' 
#' @examples
#' table.info(token_info)

table.info <- function(token_info) {
  
  table_info <- c()
  token_sentence <- token_info[[1]]
  token_word <- token_info[[2]]
  token_word_freq <- token_info[[3]]
  token_word_stem <- token_info[[4]]
  token_word_stop <- token_info[[5]]
  
  ligne <- list("number of documents", "N", length(unique(token_sentence$book)))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  ligne <- list("total number of word occurences", "M", nrow(token_word))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  ligne <- list("average number of words per document", " ", nrow(token_word)/length(unique(token_sentence$book)))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  ligne <- list("number of type words", "Mtyp", nrow(token_word_freq))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  ligne <- list("number of type words after nomalization", "Mnor", nrow(token_word_stem))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  ligne <- list("number of terms of the vocabulary", "V", nrow(token_word_stop))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  ligne <- list("average number of terms in vocabulary per document", " ", nrow(token_word_stop)/length(unique(token_sentence$book)))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  return(table_info)
}
