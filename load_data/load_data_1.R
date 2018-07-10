library(stats)
library(dplyr)
library(janeaustenr)
library(stringr)
library(tidytext)

load.data.1 <- function() {
  original_book <- austen_books() %>%
    group_by(book) %>%
    mutate(line = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                   ignore_case = TRUE)))) %>%
    ungroup()
  return(original_book[,c(1,2,4)])
}

#original_books <- load.data.1()
#if (DEBUG == TRUE) {print(original_books)}
