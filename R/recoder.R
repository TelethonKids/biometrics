#' Recode a Text File as UTF-8
#'
#' @param x path to the file
#'
#' @examples
#'
#' \dontrun{
#'
#' recoder("packages.bib")
#'
#' }
#'
#' @export
recoder <- function(x) {
  dat <- read.delim(file = x, header = F, stringsAsFactors = F, quote = "")
  write.table(dat, file = x, row.names = F, quote = F, col.names = F, fileEncoding = "UTF-8")
}
