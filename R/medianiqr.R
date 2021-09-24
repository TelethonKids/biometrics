#' medianiqr
#'
#' Return the median and interquartile range of a vector
#'
#' @param x a numeric vector
#' @param dpp (default 2) the number of decimal points to return for the median
#' @param dps (default 2) the number of decimal points to return for the interquartile range
#'
#' @return object of class character, in the form "XX.XX (XX.XX, XX.XX)"
#'
#' @importFrom stats sd
#'
#' @examples
#'   medianiqr(mtcars$mpg)
#'
#' @export
#'
medianiqr <- function(x, dpp=2, dps=2){
  if(any(class(x)!="numeric" | class(x)!="interger")) {} else{print("Check the class of the variable provided")}
  x <- as.numeric(x)
  tempsd <- sd(x, na.rm=T)
  tempn <- sum(!is.na(x))
  tempse <- tempsd / sqrt(tempn)
  paste0(sprintf(paste0("%.",dpp,"f"), quantile(x, 0.5, na.rm = T)),
         " (",
         sprintf(paste0("%.",dps,"f"), quantile(x, 0.25, na.rm = T)),
         ", ",
         sprintf(paste0("%.",dps,"f"), quantile(x, 0.75, na.rm = T)),
         ")"
  )
}
