#' meansd
#'
#' Return the mean and sd of a vector
#'
#' @param x a numeric vector
#' @param dpp (default 2) the number of decimal points to return the mean
#' @param dps (default 2) the number of decimal points to return the standard deviation
#'
#' @return A string the form of "mean (sd)" at the specified precision
#'
#' @importFrom stats sd
#'
#' @examples
#' \dontrun{
#'   data(mtcars)
#'
#'   meansd(mtcars$cyl)
#' }
#'
#' @export
meansd <- function(x, dpp=2, dps=2){
  if(any(class(x)!="numeric" | class(x)!="interger")) {} else{print("Check your format")}
  x <- as.numeric(x)
  paste0(sprintf(paste0("%.",dpp,"f"), mean(x, na.rm=T)),
         " (",
         sprintf(paste0("%.",dps,"f"), sd(x, na.rm=T)),
         ")"
  )
}
