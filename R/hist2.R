#' hist2
#'
#' Create a histogram with a normal distribution overlay in ggplot2.
#'
#' @param x a numeric vector
#'
#' @export
hist2 <- function(x) {
  dat <- tibble(x = x) %>%
    na.omit()
  ggplot(dat, aes(x = x)) +
    geom_histogram(aes(y =..density..),
                   bins = 30,
                   colour = "black",
                   fill = "white") +
    stat_function(fun = dnorm, args = list(mean = mean(dat$x), sd = sd(dat$x)))
}
