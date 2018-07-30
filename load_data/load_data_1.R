#' @description Load the data from a roman type, here any Jan Austeen's book. 
#' Put it in a tibble to use it after.
#' 
#' @param lien A character, not use here but necessary for other load.data.i with i>1
#' @return original_book A tibble with two colums. 
#' original_book$text is the lines of the text. 
#' original_book$book is the part (e.g. chapter, different book...) 
#' of the full text to which the lines belong
#' 
#' @import stats, dplyr, janeaustenr, stringr, tidytext
#' @examples
#' ## library(stats)
#' ## library(dplyr)
#' ## library(janeaustenr)
#' ## library(stringr)
#' ## library(tidytext)
#' ## original_books <- load.data.1("")

load.data.1 <- function(lien) {

  
  #for nothing just for use the argument lien
  print(lien)
  original_book <- austen_books() %>%
    group_by(book) %>%
    mutate(line = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                   ignore_case = TRUE)))) %>%
    ungroup()
  original_book1<- original_book1[,c(1,2)]
  
  return(original_book)
  
}