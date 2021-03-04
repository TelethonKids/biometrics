#' factor_table
#'
#' Takes a string of tupples and converts to a 2 column tibble of key/value pairs.
#'
#' @param x string of key/value pairs
#' @param row_sep character string separting key/value tupples separator, sent to `stringr::str_split()` (default ";")
#' @param col_sep character string sepearting keys from values, sent to `tidyr::separate()` (default "\\|")
#'
#' @importFrom magrittr `%>%`
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom dplyr mutate across
#' @importFrom tidyselect everything
#'
#' @examples
#'
#' \dontrun{
#' tmp <- "1|Some text; 2| Some more text; 3| All other text"
#' tibble(a = as.character(1:3)) %>%
#'   mutate(across("a", factor, factor_table(tmp)$key, factor_table(tmp)$value))
#' }
#'
#' @export
factor_table <- function(x, row_sep = ";", col_sep = "\\|") {

  key <- NULL

  tibble(key = str_split(x, row_sep)[[1]]) %>%
    separate(key, into = c("key", "value"), sep = col_sep) %>%
    mutate(across(everything(), trimws))

}
