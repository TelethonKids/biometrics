#' @title Combine all variable values and separated for each key group in a data frame - not sorted.
#'
#' @description
#' \code{roll_up} for grouped members of a data frame, column 1, combine all the values that are listed in all variables and separated into new columns.
#'
#' For example, combine all diagnosis values for a patient that has multiple entries in a dataset and return a new diagnosis list with each value in a defined column.
#'
#' @param x ...
#' @param vars an optional character vector of the desired output column names, this uses the \code{variables} function to create the vector. If omitted, variables names will be sequentially indexed variables of name \emph{V}.
#'
#' @return A data frame is returned with a id variable (matching the first column of the input data frame) and columns for values.
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' temp %>% group_by(root) %>%
#'     do(roll_up(., vars = c("diagnosis", "dagger", "ediag")))
#' }
#'
#' @export
roll_up <- function (x, vars = NULL) {
  id <- first(names(x)) # assumes first col is key
  y <- names(x)[names(x) != id] # defines list of columns to roll up
  o <- c()
  for (i in y) { # cycles through columns to be rolled and pulls out values as character vector
    o <- c(o, x %>% select(i) %>% pull())
  }
  o <- o[!is.na(o)] %>% unique() # finds unique values in character vector of rolled data
  o <- c(x %>% select(id) %>% unique() %>% as.character(), o) # append key id to start of vector
  num_vars <- length(o) # counts number of vectors
  # define column names
  if (is.null(vars)) {
    num_vars <- num_vars - 1
    col_names <- c(id, sprintf("V%s",seq(1:num_vars)))
  } else {
    col_names <- variables(c(id, vars), num_vars - length(vars))
    if (num_vars - length(col_names) <= 0) {
      col_names <- col_names[1:length(o)]
    }
  }
  df <- data.frame(t(o), stringsAsFactors = FALSE)
  colnames(df) <- col_names # names columns in data frame
  df
}
