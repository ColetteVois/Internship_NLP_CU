library("dplyr")


load.data.3 <- function() {

  path <- "C:/Users/rubik/Desktop/Intership_NLP_CU/Document/craft-2.0/articles/txt/"
  listfiles <- list.files(path, pattern="*.txt")
  temp <- paste(path, listfiles, sep="")
  myfiles1 <- lapply(temp, read.csv, sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)#fill = TRUE,header = FALSE,stringsAsFactors = FALSE)#, col.names = rep("text",length(listfiles)+1))
  myfiles <- myfiles1 %>% unlist %>% unlist(recursive=FALSE)
  
  original_books <- as_data_frame(myfiles)
  colnames(original_books) <- "text"

  if (DEBUG == TRUE) {print(original_books)}
  return(original_books)
}

#original_books <- load.data.3()

