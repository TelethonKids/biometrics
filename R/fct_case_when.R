#' fct_case_when
#'
#' Essentially the same as dplyr::case_when but the variable is of class factor, ordered based on the order entered into the case_when statement
#'
#' @param ... as per dplyr::case_when, essentially all input is passed through to dplyr::case_when
#'
#' <dynamic-dots> A sequence of two-sided formulas. The left hand side (LHS) determines which values match this case. The right hand side (RHS) provides the replacement value.
#'
#' The LHS must evaluate to a logical vector. The RHS does not need to be logical, but all RHSs must evaluate to the same type of vector.
#'
#' Both LHS and RHS may have the same length of either 1 or n. The value of n must be consistent across all cases. The case of n == 0 is treated as a variant of n != 1.
#'
#' NULL inputs are ignored.
#'
#' @return A vector of length 1 or n, of class factor, matching the length of the logical input or output vectors. Inconsistent lengths or types will generate an error.
#'
#' @importFrom stats sd
#'
#' @examples
#'
#'   x <- 1:50
#'   case_when(
#'     x %% 35 == 0 ~ "fizz buzz",
#'     x %% 5 == 0 ~ "fizz",
#'     x %% 7 == 0 ~ "buzz",
#'     TRUE ~ "no fizz buzz"
#'   ) %>% table
#'
#'   fct_case_when(
#'     x %% 35 == 0 ~ "fizz buzz",
#'     x %% 5 == 0 ~ "fizz",
#'     x %% 7 == 0 ~ "buzz",
#'     TRUE ~ "no fizz buzz"
#'   ) %>% table
#' }
#'
#' @export
#'
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}
