#This is the file where you can add the tokenization description

token_sentence_description <- list(c(1,sprintf("This sentence tokenisation %d is based on this package. It does that and this and is better for the sentences.",1)),
                                   c(2, sprintf("This sentence tokenisation %d is based on this package. It does that and this and is better for the sentences. But sometimes it doesn't work.",2)),
                                  c(3, sprintf("This sentence tokenisation %d is based on this package. It does that and this and is better for the sentences. But sometimes it doesn't work.",3))
                                  )
token_word_description <- list(c(1, sprintf("This word tokenisation %d is based on this package. It does that and this and is better for the words.",1)),
                               c(2, sprintf("This word tokenisation %d is based on this package. It does that and this and is better for the words.",2)),
                               c(3, sprintf("This word tokenisation %d is based on this package. It does that and this and is better for the words.",3)),
                               c(4, sprintf("This word tokenisation %d is based on this package. It does that and this and is better for the words.",4)),
                               c(5, sprintf("This word tokenisation %d is based on this package. It does that and this and is better for the words.",5))
                                )
token_norma_description <- list(c(1, sprintf("This normalization normalization tokenisation %d is based on this package. It does that and this and is better for the normalization.",1)),
                                c(2, sprintf("This normalization tokenisation %d is based on this package. It does that and this and is better for the normalization.",2)),
                                c(3, sprintf("This normalization tokenisation %d is based on this package. It does that and this and is better for the normalization.",3)),
                                c(4, sprintf("This tokenisation %d is based on this package. It does that and this and is better for the normalization.",4))
                                )

# n.tokenizer.sentence <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_sentence/", sep = "")))
# n.tokenizer.word.occu <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_word_occu/", sep=""))) - 1
# n.normalization <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/normalization/", sep=""))) - 1
# 
# token_sentence_description <- c()
# token_word_description <- c()
# token_norma_description <- c()
# for(j in 1:n.type.data){
#   
#   temp <- paste(my_path,sprintf("/Intership_NLP_CU/load_data/load_data_%d.R", j), sep="")
#   myfiles <- lapply(temp, read.csv, sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)#fill = TRUE,header = FALSE,stringsAsFactors = FALSE)#, col.names = rep("text",length(listfiles)+1))
#   
#   description <- "There is no description for this data"
#   for(i in 1:nrow(myfiles[[1]])) {
#     if(str_detect(myfiles[[1]][i,1], "@description")) {
#       description1 <- myfiles[[1]][i,1]
#       description2 <- str_replace(description1, "#' ", "")
#       description3 <- str_replace(description2, "#'", "")
#       description <- str_replace(description3, "@description ", "")
#     }
#   }
#   
#   load_data_type_description <- c(load_data_type_description,
#                                   paste(sprintf("This is the data type %d.",j), description), sep = "")
# }