#' round_vec
#'
#' A function, for vectors, to keep trailing zeroes when rounding numbers.
#'
#' @param x a numeric vector
#' @param digits integer indicating the number of decimal places to be used.
#'
#' @return object of class character"
#'
#' @import utils
#'
#' @examples
#' round_vec(mtcars$wt, 2)
#'
#'
#' @export

round_vec <- function(x, digits = 2){
  trimws(format(round(x, digits), nsmall = digits))
}


#' round_df
#'
#' A function, for data frames or tibbles, to round all numeric columns in a data frame or tibble.
#'
#' The option exists to keep these columns as numeric (simply rounding), or to convert them to characters keeping trailing zeroes in the process.
#'
#' @param x a data frame or tibble vector
#' @param digits integer indicating the number of decimal places to be used
#' @param con_char logical as to whether you want the rounded columns to be converted to character class (T) or not (F); default is F
#'
#' @return object of class tibble"
#'
#' @examples
#'
#' \dontrun{
#' round_df(iris, 0)
#'
#' round_df(iris, 0, con_char = T) %>% str
#' }
#'
#' @export

round_df <- function(x, digits = 2, con_char = F) {
  if(con_char == F){
    x %>%
      mutate_if(is.numeric, round, digits)
  } else {
    x %>%
      mutate(across(utils::globalVariables("where")(is.numeric), ~ trimws(format(round(., digits), nsmall = digits))))
  }
}
