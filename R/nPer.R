#' nPer
#'
#' Printing N and percentage
#'
#' @param vec numeric vector
#' @param intGroup xxxx
#' @param dp (default 1)
#'
#' @return xxxx
#'
#' @examples
#' \dontrun{
#'   nPer(xxx)
#' }
#'
#' @export

nPer <- function(vec, intGroup, dp = 1){
  paste0(sum(vec == intGroup, na.rm=T),
         " (",
         sprintf(paste0("%.", dp, "f"), sum(vec == intGroup, na.rm = T) / sum(!is.na(vec)) * 100),
         "%)"
  )
}

# nPer4 <- function(vec, intGroup, dp = 1){
#   paste0(sum(vec == intGroup, na.rm=T),
#          "/",
#          "sum(!is.na(vec))",
#
#          " (",
#          sprintf(paste0("%.", dp, "f"), sum(vec == intGroup, na.rm = T) / sum(!is.na(vec)) * 100),
#          "%)"
#   )
# }
