################################ USER CHANGE ############################################

#write your path to go to your file
#my_path <- "C:/Users/Projet/Intership_NLP_CU"
my_path <- "C:/Users/Projet"
#choose which data you want to load
choose_load_data <- 1
DEBUG = TRUE

################################### LOAD DATA ###########################################

source(paste(my_path, sprintf("/Intership_NLP_CU/load_data/load_data_%d.R", choose_load_data), sep = ""))
load.data.i <- sprintf("load.data.%d()", choose_load_data)
original_books <- eval(parse(text=load.data.i))
original_books <- original_books %>% mutate(rowname = 1:nrow(original_books))
original_books <- original_books[1:400,]
book_unique <- unique(original_books$book)
check_choices <- c()
for(i in book_unique){
  i_modif <- gsub(" ", "",i)
  b <- paste("Book", i_modif, sep = "" )
  check_choices <- c(check_choices, b)
}

n <- NROW(original_books)