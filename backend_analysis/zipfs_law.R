#' @description Applied Zipfs Law to the text.
#' 
#' @param token_word_freq A tibble with four colums. 
#' token_sentence$word are the words of the text in alphabetical order occuring just one. 
#' token_sentence$sentences is the list of numbers of sentences (line of the sentence in token_sentence) in which each word appear.
#' token_sentence$freq is the frequence each word appears in the text.
#' token_sentence$tf is the terme frequency of each word.
#' @return freq_by_rank
#' @return lambda
#' @return inv
#' @return summary(reg_lin)
#' 
#' @examples
#' ## heaps.law(original_books_bis, 1, 1)


zipfs.law <- function(my.texte) {
  
  # my.texte <- token_word_freq
  
  total <- sum(my.texte$freq)
  nb.mot <- nrow(my.texte)
  
  freq_by_rank <- my.texte %>% mutate(rank = row_number(), term_frequency = freq/total)
  
  #on ne prend que la partie du milieu car c est la plus lineaire  
  rank_subset <- freq_by_rank %>% filter(rank> 0.1*nb.mot, rank < 0.9*nb.mot) # entre 10% et 90%
  reg_lin <- lm(log10(rank_subset$term_frequency) ~ log10(rank_subset$rank))
  
  lambda <- exp(reg_lin$coefficients[[1]])
  inv <- reg_lin$coefficients[[2]]   #environ -1
  summary(reg_lin)
  
  return(c(list(freq_by_rank), list(lambda), list(inv), list(summary(reg_lin))))
}

#zipfs.law(token_word_freq)
