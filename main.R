################################ USER CHANGE ############################################

#Write your path to go to your file. 
#This path should be the path before the folder you just downloaded on GitHub.
#For instance, if the path to the folder "app" in the folder you downloaded on GitHub is "C:/Users/Projet/Intership_NLP_CU/app"
#Then you should put "C:/Users/Projet" as my_path.

my_path <- "C:/Users/Projet/Intership_NLP_CU"
# my_path <- "C:/Users/rubik/Desktop"

#choose which data you want to load
choose_load_data <- 3
DEBUG = TRUE

################################# Load the libraries and install the packages if not  #################################################

lien <- paste(my_path,"/Intership_NLP_CU/install_packages.R", sep="")
source(lien)

####################################   Doing the choices for the check boxes  ##########################################################
n.tokenizer.sentence <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_sentence/", sep = "")))
n.tokenizer.word <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_word/", sep="")))
n.normalization <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/normalization/", sep=""))) - 1
n.type.data <- length(list.files(paste(my_path,"/Intership_NLP_CU/load_data/", sep="")))


check_choices_token_sentence_check <- c()
check_choices_token_word_check <- c()
check_choices_token_norma_check <- c()
check_choices_load_data <- c()

count_1 = 1
while(count_1 <= n.tokenizer.sentence){
  b_paste_local <- toString(count_1)
  check_choices_token_sentence_check <- c(check_choices_token_sentence_check, b_paste_local)
  count_1 =count_1 +1
}
count_2 = 1
while(count_2 <= n.tokenizer.word){
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
n <- 20

#########################################################  Running the shiny app ########################################################

lien <- paste(my_path,"/Intership_NLP_CU/app", sep="")
runApp(lien, launch.browser = TRUE)