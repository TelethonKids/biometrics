#' roundf
#'
#' A function to keep trailing zeroes when rounding numbers.
#'
#' @param x a numeric vector
#' @param digits integer indicating the number of decimal places to be used.
#'
#' @return object of class character"
#'
#' @examples
#' roundf(mtcars$wt, 2)
#' }
#'
#' @export

roundf <- function(x, digits = 0){
  sprintf(paste0("%.",digits,"f"), round(x,digits))
}
