#' hist2
#'
#' Create a histogram with a normal distribution overlay in ggplot2.
#'
#' @param x a numeric vector
#'
#' @importFrom ggplot2 ggplot geom_freqpoly stat_function
#' @importFrom stats dnorm na.omit
#'
#' @export
hist2 <- function(x) {

  ..density.. <- NULL

  dat <- na.omit(tibble(x = x))

  ggplot(dat, aes(x = x)) +
    geom_histogram(aes(y = ..density..),
                   bins = 30,
                   colour = "black",
                   fill = "white") +
    stat_function(fun = dnorm, args = list(mean = mean(dat$x), sd = sd(dat$x)))
}
