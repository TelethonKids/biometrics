#' Rounds the double columns in a data frame
#'
#' All columns in a data frame with class double will be rounded to the sepecified
#' number of digits. Integer classes will be left unchanged.
#'
#' @param df a data frame object
#' @param digits the number of digits after the decimal point
#'
#' @return the data frame with rounded numbers
#'
#' @examples
#'
#' df <- data.frame(a = "a", b = 0.025, c = 15.34234, d = T)
#'
#' round_df(df, 2)
#'
#' @export
round_df <- function(df, digits) {
  nums <- vapply(df, is.double, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  df
}
