#' contingency_table
#' 
#' returns a frequency table of two vectors with both counts and proportions.
#' 
#' @param vec1 a vector - should be discrete values
#' @param vec2 a vector - should be discrete values
#' @dp number of decimal places in proportion
#' 
#' @return object of class matrix
#' 
#' @import janitor
#' 
#' @examples 
#' contingency_table(mtcars$gear, mtcars$cyl)

contingency_table <- function(vec1, vec2, dp = 2) {
  
  tab <- addmargins(table(vec1, vec2))
  ptab <- addmargins(prop.table(table(vec1, vec2)))
  
  matrix(
    paste0(tab, " (", round(ptab, dp), ")"),
    nrow = nrow(tab),
    dimnames = list(rownames(tab), colnames(tab))
  )
}
