#' quick_text
#'
#' Improvements to biometrics::format_model.
#'
#' @param mod_list a list of regression models
#' @param effects a broom::tidy option e.g. "fixed", "random"
#' @param conf.int a broom::tidy option to generate confidence intervals, logical
#' @param exponentiate a broom::tidy optoin, logical
#' @param conf.level the reported confidence interval of model estimates
#' @param dp the number of decimal points reported in returned strings
#'
#' @return a list of tibbles generated from broom:tidy.
#' @return text1: estimate (95\% CI: lower to upper)
#' @return text2: (estimate; 95\% CI: lower to upper)
#' @return text3 (for tables): estimate <br /> CI [lower, upper]
#'
#' @importFrom purrr map map_dfr
#' @importFrom broom tidy
#'
#' @seealso \link[=format_text]{format_text()}
#'
#' @export
quick_text <- function(mod_list, effects = NULL, conf.int = TRUE, exponentiate = FALSE, conf.level = 0.95, dp = 2) {

  map(mod_list,
      tidy,
      effects = effects,
      conf.int = conf.int,
      conf.level = conf.level,
      exponentiate = exponentiate) %>%
    map_dfr(format_text, conf.level, dp, .id = "outcome")

}

#' format_text
#'
#' Converts model output from quick_text() into a tibble with 3 pre-formatted options.
#'
#' @param x a tibble with at least columns named estimate, conf.low and conf.high
#' @param conf.level the label describing the calculated confidence level
#' @param dp the number of decimal places to report
#'
#' @return text1: estimate (95\% CI: lower to upper)
#' @return text2: (estimate; 95\% CI: lower to upper)
#' @return text3 (for tables): estimate <br /> CI [lower, upper]
#'
#' @importFrom dplyr mutate
#'
#' @seealso \link[=quick_text]{quick_text()}
#'
#' @export
format_text <- function(x, conf.level = 0.95, dp = 2) {
  x %>%
    biometrics::round_df(dp) %>%
    mutate(text1 = paste0(.data$estimate,
                                 " (", 100 * conf.level, "% CI: ", .data$conf.low,
                                 " to ", .data$conf.high, ")"),
                  text2 = paste0("(",
                                 .data$estimate, "; ", 100 * conf.level, "% CI: ",
                                 .data$conf.low, " to ", .data$conf.high, ")"),
                  text3 = paste0(.data$estimate, "<br />CI [", .data$conf.low, ", ", .data$conf.high,"]"))
}
