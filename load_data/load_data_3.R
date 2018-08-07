#' Load the data from the file which has been uploaded in the app.
#' Put it in a tibble to use it after.
#' @description Usually use to load medical type data. 
#' 
#' @param lien A character, the path to the data
#' @return original_book A tibble with two colums. 
#' original_book$text is the lines of the text. 
#' original_book$book is the part (e.g. chapter, different book...) 
#' of the full text to which the lines belong
#' 
#' @import dplyr, stringr
#' @examples
#' ## library(dplyr)
#' ## library(stringr)
#' ## original_books <- load.data.2("C:/Users/Projet/AppData/Local/Temp/RtmpGojZEM/fe9627ff0cf9fe33f4cb4e0a/0.txt")

load.data.3 <- function(path) {

  print(path)
  # path <- "C:/Users/rubik/Desktop/Document/pestian_suicide_notes/i2b2/"
  # path <- "C:/Users/rubik/Desktop/Document/lastwords-master/last_words_final_version/"
  
  path <- gsub("//","/", path, fixed=TRUE)
  path <- paste0(path, "/")
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

#path <- "C:/Users/rubik/Desktop/Internship_NLP_CU/Document/craft-2.0/articles/txt/"
#path <- "C:/Users/rubik/Desktop/Document/craft-2.0/articles/txt/"
#path <- "C:/Users/rubik/Desktop/Document/pestian_suicide_notes/i2b2/"

#original_books <- load.data.3("C:/Users/rubik/Desktop/Document/pestian_suicide_notes/i2b2/")
