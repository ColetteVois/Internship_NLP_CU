load.data.2 <- function() {
  twitter <- read.csv("C:/Users/rubik/Desktop/Intership_NLP_CU/Document/Twitter-Data/Twitter-Data/#cancer+smoking.csv", sep=",", encoding = "UTF-8", header = FALSE, col.names = "text", stringsAsFactors = FALSE)
  original_books <- as_data_frame(twitter)
  #original_books <- as.character(original_books1)

  if (DEBUG == TRUE) {original_books}
  return(original_books)
}

original_books <- load.data.2()
if (DEBUG == TRUE) {print(original_books)}
