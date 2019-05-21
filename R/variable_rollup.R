#' @title Roll column values from multiple data table rows into a single record.
#'
#' @description
#' \code{variable_rollup} collects all values in a data table and returns them in a single row of a size matching the number of unique values.\cr\cr The column names of the input data frame can be maintained, or new labels can be supplied; values are clipped When the number of columns are fewer than the number of rolled values.
#'
#' @param x Input data frame that contains all rows and columns that are to be rolled.
#' @param names An optional vector of output column names.
#' @param by An optional vector of elements that are listed in the order required of the output.
#' @param clip When including a \code{by} argument, \code{clip = TRUE} will return only values that are also in the \code{by} vector, otherwise the remaining codes are appended to the end of the sorted list; default: \code{clip = FALSE}.
#'
#' @return A single row data table.\cr\cr NAs are removed and output strings are not factors.
#'
#' @import dplyr
#' @import tibble
#'
#' @examples
#' \dontrun{
#' data.frame(a = c(1, 2), b = c(2, 3), c = c(NA, 4)) %>% tbl_df -> x
#' data.frame(a = c(NA, NA), b = c(NA, NA), c = c(NA, NA)) -> y
#' LETTERS -> z
#' c(4, 3) -> s
#'
#'# Column names are not specified
#' variable_rollup(x) # Will warn that there are too few columns for the number of variables
#'
#'# Sepcify column names
#' variable_rollup(x, names = z)
#'
#'# Data frame that contains only NAs
#' variable_rollup(y)
#'
#' # Sort output per s and append remaining values not in s.
#' variable_rollup(x, names = z, by = s, clip = FALSE)
#' }
#'
#' @export
variable_rollup <- function(x, names = NULL, by = NULL, clip = FALSE) {

  x %>%
    as.matrix() %>%
    as.vector() ->
    codes

  codes[!is.na(codes)] %>%
    unique() ->
    codes

  if(is.null(names)) {
    x %>% names() ->
      names
  }

  if(!is.null(by)) {
    codes[order(match(codes, by))] ->
      sorted
    if(clip) {
      sorted[sorted %in% by] ->
        codes
    } else {
      sorted ->
        codes
    }
  }

  if(length(names) < length(codes)) {
    codes[1:length(names)] -> codes
    warning("Too few specified columns, output has been clipped.")
  }

  if(length(codes) >= 1) {
    data.frame(names = names[1:length(codes)], codes = codes, stringsAsFactors = FALSE) %>%
      tbl_df() %>%
      spread(names, codes) ->
      df
  } else {
    data.frame(NA) %>%
      tbl_df() ->
      df
    names(df) <- names[1]
  }

  df
}
