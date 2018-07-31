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

n.tokenizer.sentence <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_sentence/", sep = "")))
n.tokenizer.word.occu <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/tokenizer_word_occu/", sep=""))) - 1
n.normalization <- length(list.files(paste(my_path,"/Intership_NLP_CU/preprocessing/normalization/", sep=""))) - 1

token_sentence_description <- c()
token_word_description <- c()
token_norma_description <- c()

#Description of the sentence tokenization
for(j in 1:n.tokenizer.sentence){

 temp <- paste(my_path,sprintf("/Intership_NLP_CU/preprocessing/tokenizer_sentence/tokenizer_sentence_%d.R", j), sep="")
 myfiles <- lapply(temp, read.csv, sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)#fill = TRUE,header = FALSE,stringsAsFactors = FALSE)#, col.names = rep("text",length(listfiles)+1))
 description <- "There is no description available for this sentence tokenization. "
 for(i in 1:nrow(myfiles[[1]])) {
   if(str_detect(myfiles[[1]][i,1], "@description")) {
     description1 <- myfiles[[1]][i,1]
     description2 <- str_replace(description1, "#' ", "")
     description3 <- str_replace(description2, "#'", "")
     description <- str_replace(description3, "@description ", "")
   }
 }
 token_sentence_description <- c(token_sentence_description,
                                 paste0(sprintf("This is the description of the sentence tokenization %d. ",j), description))
}

#Description of the word tokenization
for(j in 1:n.tokenizer.word.occu){

  temp <- paste(my_path,sprintf("/Intership_NLP_CU/preprocessing/tokenizer_word_occu/tokenizer_word_occu_%d.R", j), sep="")
  myfiles <- lapply(temp, read.csv, sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)#fill = TRUE,header = FALSE,stringsAsFactors = FALSE)#, col.names = rep("text",length(listfiles)+1))
  description <- "There is no description available for this word tokenization. "
  for(i in 1:nrow(myfiles[[1]])) {
    if(str_detect(myfiles[[1]][i,1], "@description")) {
      description1 <- myfiles[[1]][i,1]
      description2 <- str_replace(description1, "#' ", "")
      description3 <- str_replace(description2, "#'", "")
      description <- str_replace(description3, "@description ", "")
    }
  }
  token_word_description <- c(token_word_description,
                                  paste0(sprintf("This is the description of the word tokenization %d. ",j), description))
}

#Description of the normalization tokenization
for(j in 1:n.normalization){

  temp <- paste(my_path,sprintf("/Intership_NLP_CU/preprocessing/normalization/normalization_%d.R", j), sep="")
  myfiles <- lapply(temp, read.csv, sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)#fill = TRUE,header = FALSE,stringsAsFactors = FALSE)#, col.names = rep("text",length(listfiles)+1))
  description <- "There is no description available for this normalization tokenization. "
  for(i in 1:nrow(myfiles[[1]])) {
    if(str_detect(myfiles[[1]][i,1], "@description")) {
      description1 <- myfiles[[1]][i,1]
      description2 <- str_replace(description1, "#' ", "")
      description3 <- str_replace(description2, "#'", "")
      description <- str_replace(description3, "@description ", "")
    }
  }
  token_norma_description <- c(token_norma_description,
                                  paste0(sprintf("This is the description of the normalization tokenization %d. ",j), description))
}