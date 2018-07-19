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
#Removing the spaces from the column book. I had to change the type to character in order to change the column and 
#then re change it in factor to let it as it was before.
count = 1
original_books$book <- as.character(original_books$book)
for(i in original_books$book){
  original_books$book[count] <- gsub(" ", "",i)
  count = count +1
}
original_books$book <- as.factor(original_books$book)

#Doing the choices for the check box
book_unique <- unique(original_books$book)
check_choices <- c()
for(i in book_unique){
  b <- paste("Book", i, sep = "" )
  check_choices <- c(check_choices, b)
}

n <- NROW(original_books)