#' Round data frame decimal points
#'
#' Round the numeric columns of a data frame to the specified number of decimal
#' points using the `sprintf` function.
#'
#' @param df the data frame to be rounded
#' @param dp (default 2) the desired number of decimal points
#'
#' @return a data frame, the rounded columns are convertd to character objects.
#'
#' @importFrom dplyr mutate_at
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#'
#' head( round_df(mtcars, 1) )
#' }
#'
#' @export
round_df <- function(df, dp = 2) {
  mutate_at(df,
            names(df)[vapply(df, is.numeric, FUN.VALUE = logical(1), USE.NAMES = F)],
            .funs = list(~sprintf(paste0("%0.", dp, "f"), .)))
}
