
zipfs.law <- function(my.texte) {
  
  my.texte <- token_word_freq
  
  total <- sum(my.texte$freq)
  nb.mot <- nrow(my.texte)
  
  freq_by_rank <- my.texte %>% mutate(rank = row_number(), term_frequency = freq/total)
  
  
  #on ne prend que la partie du milieu car c est la plus lineaire  

  rank_subset <- freq_by_rank %>% filter(rank> 0.1*nb.mot, rank < 0.9*nb.mot) # entre 10% et 90% peut être changer ?
  reg_lin <- lm(log10(rank_subset$term_frequency) ~ log10(rank_subset$rank))
  
  lambda <- exp(reg_lin$coefficients[[1]])
  inv <- reg_lin$coefficients[[2]]   #environ -1
  summary(reg_lin)
  
  return(c(list(freq_by_rank), list(lambda), list(inv), list(summary(reg_lin))))

  # 
  # jpeg(paste(my_path, sprintf('/Intership_NLP_CU/boxplot/zipfs_law_data_%d.jpg',choose_load_data),sep =""))
  # freq_by_rank %>% ggplot(aes(rank, term_frequency)) +
  #   geom_abline(intercept = reg_lin$coefficients[[1]], slope = inv, color = "red") +
  #   geom_line(size = 1.1, alpha = 0.8, show.legend= FALSE) +
  #   scale_x_log10() +
  #   scale_y_log10()
  # dev.off()
  
}

#zipfs.law(token_word_freq)
