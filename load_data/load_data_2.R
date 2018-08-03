#' Load the data from the file which has been uploaded in the app.
#' @description Usually use to load twitter type data. 
#' Put it in a tibble to use it after.
#' 
#' @param lien A character, the path to the data
#' @return original_book A tibble with two colums. 
#' original_book$text is the lines of the text. 
#' original_book$book is the part (e.g. chapter, different book...) 
#' of the full text to which the lines belong
#' 
#' @examples
#' ## original_books <- load.data.2("C:/Users/Projet/AppData/Local/Temp/RtmpGojZEM/fe9627ff0cf9fe33f4cb4e0a/0.txt")


load.data.2 <- function(path) {
  
  path1 <- gsub("\\","/", path, fixed=TRUE)
  twitter <- read.csv(path1, sep=",", encoding = "UTF-8", header = FALSE, col.names = "text", stringsAsFactors = FALSE)
  #twitter2 <- read.csv("C:/Users/Projet/Desktop/Internship/Data/Twitter-Data/cancer+smoking.csv", sep=",", encoding = "UTF-8", header = FALSE, col.names = "text", stringsAsFactors = FALSE)
  
  original_books <- as_data_frame(twitter)    #dplyr::bind_rows(twitter,twitter2))

  return(original_books)
}
