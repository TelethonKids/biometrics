#' @title Create a string vector with a numbered repeating final element.
#'
#' @description
#' \code{variables} returns a string vector where the last entry is repeated by a specified number of times with appended incremental numbering.
#'
#' @param names string vector of any length, the last entry will be repeated \code{n} times with sequential labelling starting at 1.
#' @param n integer indicating the number of times that the last entry of \code{names} shall be repeated.
#'
#' @examples
#' variables(c("a", "b", "c"), 20)
#'
#' @export
variables <- function(names, n) {
  x <- names[-length(names)]

  for(i in 1:n){
    x <- c(x, paste0(names[length(names)],i))
  }

  x
}
