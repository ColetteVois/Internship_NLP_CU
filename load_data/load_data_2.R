load.data.2 <- function(lien) {
  
  lien <- gsub("\\","/", lien, fixed=TRUE)
  # twitter <- read.csv("C:/Users/rubik/Desktop/Intership_NLP_CU/Document/Twitter-Data/Twitter-Data/#cancer+smoking.csv", sep=",", encoding = "UTF-8", header = FALSE, col.names = "text", stringsAsFactors = FALSE)
  twitter <- read.csv(lien, sep=",", encoding = "UTF-8", header = FALSE, col.names = "text", stringsAsFactors = FALSE)
  #twitter2 <- read.csv("C:/Users/Projet/Desktop/Internship/Data/Twitter-Data/cancer+smoking.csv", sep=",", encoding = "UTF-8", header = FALSE, col.names = "text", stringsAsFactors = FALSE)
  
  original_books <- as_data_frame(twitter)    #dplyr::bind_rows(twitter,twitter2))
  #original_books <- as.character(original_books1)

  if (DEBUG == TRUE) {original_books}
  return(original_books)
  
}

#original_books <- load.data.2()
#if (DEBUG == TRUE) {print(original_books)}
