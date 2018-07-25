#' Add together two numbers.
#' 
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)



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
  
  ligne <- list("number total occurences of words", "M", nrow(token_word))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  ligne <- list("means number words per document", " ", nrow(token_word)/length(unique(token_sentence$book)))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  ligne <- list("number of type words", " ", nrow(token_word_freq))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  ligne <- list("number of type words after steaming", "My", nrow(token_word_stem))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  ligne <- list("number of terme in vocabulary", " ", nrow(token_word_stop))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  ligne <- list("mean number of terme in vocabulary per document", " ", nrow(token_word_stop)/length(unique(token_sentence$book)))
  names(ligne) <- c("Variables","Symboles", "Values")
  table_info <- dplyr::bind_rows(table_info,ligne)
  
  return(table_info)
}
