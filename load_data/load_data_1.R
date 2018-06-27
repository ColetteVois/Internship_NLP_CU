library(dplyr)
library(janeaustenr)
library(stringr)
library(tidytext)

DEBUG = TRUE

#---------------------------------MAIN-----------------------------------#

#################################MAIN 1 AUSTEN

load.data.1 <- function() {
  original_books <- austen_books() %>%
    group_by(book) %>%
    mutate(line = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                   ignore_case = TRUE)))) %>%
    ungroup()
  
  if (DEBUG == TRUE) {original_books}
  return(original_books)
}
