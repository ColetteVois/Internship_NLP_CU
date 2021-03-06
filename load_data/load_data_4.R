library(stringr)

load.data.4 <- function(path) {
  
  path <- "C:/Users/rubik/Desktop/Document/pestian_suicide_notes/i2b2/"
  listfiles <- list.files(path, pattern="*.txt")
  temp <- paste(path, listfiles, sep="")
  myfiles1 <- lapply(temp, read.csv, sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)#fill = TRUE,header = FALSE,stringsAsFactors = FALSE)#, col.names = rep("text",length(listfiles)+1))
  myfiles2 <- myfiles1 %>% unlist(recursive=FALSE) #myfiles1 %>% unlist %>% unlist(recursive=FALSE)
  taille_doc <- c()
  for(i in 1:length(myfiles2)) {
    taille_doc[i] <- length(myfiles2[[i]])
  }
  myfiles <- myfiles2 %>% unlist(recursive=FALSE)
  
  
  original_books1 <- as_data_frame(myfiles)
  colnames(original_books1) <- "text"
  
  original_books <- original_books1 %>% mutate(book = rep(str_sub(listfiles,1,-5),taille_doc))
  
  return(original_books)
  
}