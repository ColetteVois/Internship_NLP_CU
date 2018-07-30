################################ USER CHANGE ############################################

#Write your path to go to your file. 
#This path should be the path before the folder you just downloaded on GitHub.
#For instance, if the path to the folder "app" in the folder you downloaded on GitHub is "C:/Users/Projet/Intership_NLP_CU/app"
#Then you should put "C:/Users/Projet" as my_path.

#my_path <- "C:/Users/Projet/Intership_NLP_CU"
my_path <- "C:/Users/rubik/Desktop"

#choose which data you want to load
choose_load_data <- 3
DEBUG = TRUE

################################# Load the libraries and install the packages if not  #################################################

#lien <- paste(my_path,"/Intership_NLP_CU/install_packages.R", sep="")
#source(lien)

################################### LOAD DATA ###########################################

source(paste(my_path, sprintf("/Intership_NLP_CU/load_data/load_data_%d.R", choose_load_data), sep = ""))
load.data.i <- sprintf("load.data.%d()", choose_load_data)
original_books <- eval(parse(text=load.data.i))
original_books <- original_books %>% mutate(rowname = 1:nrow(original_books), book = "all the same")
original_books_bis <- original_books[1:110,]
#Removing the spaces from the column book. I had to change the type to character in order to change the column and 
#then re change it in factor to let it as it was before.
count = 1
original_books_bis$book <- as.character(original_books_bis$book)
for(i in original_books_bis$book){
  original_books_bis$book[count] <- gsub(" ", "",i)
  count = count +1
}
original_books_bis$book <- as.factor(original_books_bis$book)


n <- NROW(original_books_bis)


####################################   Doing the choices for the check boxes  ##########################################################
n.tokenizer.sentence <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_sentence/", sep = "")))
n.tokenizer.word.occu <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_word_occu/", sep=""))) - 1
n.normalization <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/normalization/", sep=""))) - 1
n.type.data <- length(list.files(paste(my_path,"/Intership_NLP_CU/load_data/", sep="")))

book_unique <- unique(original_books_bis$book)
check_choices <- c()
check_choices_token_sentence_check <- c()
check_choices_token_word_check <- c()
check_choices_token_norma_check <- c()
check_choices_load_data <- c()
for(i in book_unique){
  a_paste_local <- paste("Book", i, sep = "" )
  check_choices <- c(check_choices, a_paste_local)
}
count_1 = 1
while(count_1 <= n.tokenizer.sentence){
  b_paste_local <- toString(count_1)
  check_choices_token_sentence_check <- c(check_choices_token_sentence_check, b_paste_local)
  count_1 =count_1 +1
}
count_2 = 1
while(count_2 <= n.tokenizer.word.occu){
  c_paste_local <- toString(count_2)
  check_choices_token_word_check <- c(check_choices_token_word_check, c_paste_local)
  count_2 =count_2 +1
}
count_3 = 1
while(count_3 <= n.normalization){
  d_paste_local <- toString(count_3)
  check_choices_token_norma_check <- c(check_choices_token_norma_check, d_paste_local)
  count_3 =count_3 +1
}
count_4 = 1
while(count_4 <= n.type.data){
  e_paste_local <- toString(count_4)
  check_choices_load_data <- c(check_choices_load_data, e_paste_local)
  count_4 =count_4 +1
}

m <- 400

#########################################################  Running the shiny app ########################################################

lien <- paste(my_path,"/Intership_NLP_CU/app", sep="")
runApp(lien, launch.browser = TRUE)