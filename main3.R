library(tibble)

################################ USER CHANGE ############################################

#write your path to go to your file
my_path <- "C:/Users/rubik/Desktop/Intership_NLP_CU"
#choose which data you want to load
choose_load_data <- 3
#choose your tokenizer 
choose_tokenizer_word <- 1
DEBUG <- TRUE

################################ LOAD DATA ##############################################

source(paste(my_path, sprintf("/Intership_NLP_CU/load_data/load_data_%d.R", choose_load_data), sep = ""))
load.data.i <- sprintf("load.data.%d()", choose_load_data)
original_books <- eval(parse(text=load.data.i))
book.length <- nrow(original_books)

################################# TOKENIZER SENTENCE ###################################

lien <- paste(my_path, sprintf("/Intership_NLP_CU/preprocessing/tokenizer_word_occu/tokenizer_word_occu_%d.R", choose_tokenizer_word), sep="")
source(lien)

original_books2 <- as_tibble(original_books)
tokenizer.word.i <- sprintf("tokenizer.word.%d(original_books2)", choose_tokenizer_word)

token_word <- eval(parse(text=tokenizer.word.i))
total <- sum(token_word[2])
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