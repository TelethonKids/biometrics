#' nPer
#'
#' A helper function for use in descriptive statistics tables.
#'
#' Prints N and percentage for categorical variables (accepts vector of responses)
#'
#' @param vec numeric vector
#' @param intGroup the response to be searched for (integer, character string, or vector of responses)
#' @param dp (default 1)
#'
#' @return object of class character, in the form "N (XX.X%)"
#'
#' @examples
#' table(iris$Species)
#'
#' nPer(iris$Species, "setosa")
#' nPer(iris$Species, c("versicolor", "virginica"))
#'   nPer(xxx)
#' }
#'
#' @export

nPer <- function(vec, intGroup, dp = 1){
  paste0(sum(vec %in% intGroup, na.rm=T),
         " (",
         sprintf(paste0("%.", dp, "f"), sum(vec == intGroup, na.rm = T) / sum(!is.na(vec)) * 100),
         "%)"
  )
}
