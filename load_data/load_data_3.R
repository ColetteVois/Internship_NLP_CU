#################################MAIN 3 ARTICLE SCIENTIFIQUE

load.data.3 <- function() {

  article1 <- read.csv("C:/Users/rubik/Desktop/Intership_NLP_CU/Document/craft-2.0/articles/txt/11532192.txt", sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)
  article2 <- read.csv("C:/Users/rubik/Desktop/Intership_NLP_CU/Document/craft-2.0/articles/txt/11597317.txt", sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)
  article3 <- read.csv("C:/Users/rubik/Desktop/Intership_NLP_CU/Document/craft-2.0/articles/txt/11897010.txt", sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)
  article4 <- read.csv("C:/Users/rubik/Desktop/Intership_NLP_CU/Document/craft-2.0/articles/txt/12079497.txt", sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)

  article <- merge(article1,article2,all=TRUE)
  article <- merge(article,article3,all=TRUE)
  article <- merge(article,article4,all=TRUE)

  original_books <- as_data_frame(article)

  if (DEBUG == TRUE) {original_books}
  return(original_books)
}