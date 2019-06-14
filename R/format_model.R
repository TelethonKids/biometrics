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
#' @importFrom dplyr case_when mutate mutate_at select left_join
#' @importFrom kableExtra cell_spec
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
  colours_3pal <- c("#8F715B", "#F1931B", "#D6618F") # negative, not significant, positive
  broom::tidy(x, effects = effects) %>%
    dplyr::mutate(conf.low = estimate - std.error * qnorm((1 + conf.level) / 2),
                  conf.high = estimate + std.error * qnorm((1 + conf.level) / 2),
                  estimate_tmp = estimate,
                  conf.low_tmp = conf.low,
                  conf.high_tmp = conf.high) %>%
    dplyr::mutate_at(c("estimate_tmp", "conf.low_tmp", "conf.high_tmp"), .funs = list(~sprintf(paste0("%0.", dp, "f"), .))) %>%
    dplyr::mutate(text1 = paste0(.data$estimate_tmp, " (", 100 * conf.level, "% CI: ", .data$conf.low_tmp, " to ", .data$conf.high_tmp, ")"),
                  text2 = paste0("(", .data$estimate_tmp, "; ", 100 * conf.level, "% CI: ", .data$conf.low_tmp, " to ", .data$conf.high_tmp, ")")) %>%
    dplyr::select(-(estimate_tmp:conf.high_tmp)) %>%
    dplyr::mutate(conf.low_tmp = conf.low, conf.high_tmp = conf.high) %>%
    dplyr::mutate_at(c("conf.low_tmp", "conf.high_tmp"), list(~(. >= 0))) %>% # is interval terminus positive?
    dplyr::mutate(ci.significant = conf.low_tmp == conf.high_tmp,
                  styled = kableExtra::cell_spec(text1, color = dplyr::case_when(!ci.significant ~ colours_3pal[2],
                                                                                 estimate >= 0 ~ colours_3pal[3],
                                                                                 estimate < 0 ~ colours_3pal[1],
                                                                                 T ~ "")),
                  p.stars = dplyr::case_when(p.value < 0.001 ~ "***",
                                             p.value < 0.01 ~ "**",
                                             p.value < 0.05 ~ "\U2217",
                                             p.value < 0.1 ~ ".",
                                             T ~ "")) %>%
    dplyr::select(-(conf.low_tmp:conf.high_tmp))
}
