#' Count and Sort Observations by Multiple Group Variables
#'
#' This function groups the data by the specified variables, counts the number of observations
#' in each group, and then sorts these groups in descending order based on the count.
#'
#' @param data A data frame to be processed.
#' @param ... Grouping variables passed to dplyr::group_by.
#'
#' @return A data frame with the counts of each group, sorted in descending order.
#' @export
#' @examples
#' df <- data.frame(
#'   x = sample(LETTERS[1:5], 100, replace = TRUE),
#'   y = sample(1:3, 100, replace = TRUE)
#' )
#' count_and_sort(df, x, y)
count_and_sort <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(n = n(), .groups = 'drop') %>%
    arrange(desc(n))
}
