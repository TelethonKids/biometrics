#' contingency_table
#'
#' returns a frequency table of two vectors with both counts and proportions.
#'
#' @param vec1 a vector - should be discrete values
#' @param vec2 a vector - should be discrete values
#' @param dp number of decimal places in proportion
#'
#' @return object of class matrix
#'
#' @examples#'
#' \dontrun{ contingency_table(mtcars$gear, mtcars$cyl) }#'
#' @export

contingency_table <- function(vec1, vec2, dp = 1) {

  tab <- addmargins(table(vec1, vec2))
  ptab <- addmargins(prop.table(table(vec1, vec2)))

  matrix(
    paste0(tab, " (", round(100*ptab, dp), "%)"),
    nrow = nrow(tab),
    dimnames = list(rownames(tab), colnames(tab))
  )
}
