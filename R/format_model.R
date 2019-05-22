#' Format a model for quick in-text referencing
#'
#' This function builds on `broom::tidy()` by adding formatted strings ready for use in text.
#'
#' @param x a model object
#' @param effects (default NULL) a tidy effects option e.g. "fixed", "random"
#' @param conf.level (default 0.95) the reported confidence interval of model estimates
#' @param dp (default 2) the number of decimal points reported in returned strings
#'
#' @return a tibble with default `tidy()` parameters plus `conf.low`, `conf.high`, `text1`, and `text2`
#'
#' @importFrom broom tidy
#' @importFrom dplyr mutate select left_join
#'
#' @examples
#' \dontrun{
#' data(iris)
#' m1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#'
#' format_model(m1) %>% select(term, estimate, conf.low, conf.high)
#'
#' format_model(m1)$text1
#' format_model(m1)$text2
#' }
#'
#' @export
format_model <- function(x, effects = NULL, conf.level = 0.95, dp = 2) {
  a <- broom::tidy(x, effects = effects) %>%
    mutate(conf.low = estimate - std.error * qnorm((1 + conf.level) / 2),
           conf.high = estimate + std.error * qnorm((1 + conf.level) / 2))
  b <- a %>%
    round_df(dp) %>%
    dplyr::mutate(text1 = paste0(.data$estimate, " (", 100 * conf.level, "% CI: ", .data$conf.low, " to ", .data$conf.high, ")"),
                  text2 = paste0("(", .data$estimate, "; , ", 100 * conf.level, "% CI: ", .data$conf.low, " to ", .data$conf.high, ")")) %>%
    dplyr::select(.data$term, .data$text1, .data$text2)
  dplyr::left_join(a, b, by = "term")
}
