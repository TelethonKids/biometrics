#' A data table of alpha/numeric key/value pairs, where key is the
#' letter sequence of the excel column. Range of letter sequence is A - ZZZ.
#'
#' Usage: excel_ref["ABC"]$numbers
#'
#' Created by Paul Stevenson 21-May-2019.
#'

#### library ----

library(devtools)
library(dplyr)
library(data.table)

excel_ref <- bind_rows(

  tibble(letters = LETTERS),

  expand.grid(LETTERS, LETTERS, stringsAsFactors = F) %>%
    as_tibble() %>%
    mutate(letters = paste0(Var2, Var1)) %>%
    select(letters),

  expand.grid(LETTERS, LETTERS, LETTERS, stringsAsFactors = F) %>%
    as_tibble() %>%
    mutate(letters = paste0(Var3, Var2, Var1)) %>%
    select(letters)

) %>%
  mutate(numbers = 1:nrow(.)) %>%
  as.data.table()

setkey(excel_ref, letters)

#### save ----

devtools::use_data(excel_ref, overwrite = T)
