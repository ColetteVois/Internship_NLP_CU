
zipfs.law <- function(my.texte) {
  
  my.texte <- token_word
  
  total <- nrow(token_word)
  
  freq_by_rank <- token_word %>% mutate(rank = row_number(), term_frequency = n/total)
  
  
  #on ne prend que la partie du milieu car c est la plus lineaire  
  rank_subset <- freq_by_rank %>% filter(rank<500, rank > 10) # 10 et 500 peut etre changer
  
  reg_lin <- lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
  
  lambda <- exp(reg_lin$coefficients[[1]]) 
  inv <- reg_lin$coefficients[[2]]   #environ -1
  summary(reg_lin)
  
  jpeg(paste(my_path, sprintf('/Intership_NLP_CU/boxplot/zipfs_law_data_%d.jpg',choose_load_data),sep =""))
  freq_by_rank %>% ggplot(aes(rank, term_frequency)) + 
    geom_abline(intercept = reg_lin$coefficients[[1]], slope = inv, color = "red") + 
    geom_line(size = 1.1, alpha = 0.8, show.legend= FALSE) + 
    scale_x_log10() + 
    scale_y_log10()
  dev.off()
  
}

zipfs.law(token_word)