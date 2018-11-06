# roxygen2::roxygenize()

#' Create Bootstrap 4 tooltip with an abbreviation and its definition
#'
#' @param x the abbreviation, as string, matching the abbreviation data.table key
#' @param y the abbreviation data.table
#'
#' @return a html anchor (i.e. <a ...>KEY</a>) defining a tooltip with the abbreviation's definition (defined in data.table column "def").
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#' abbrv <- data.table(abbrv = c("PGS"), def = c("Paul G Stevenson"), key = "abbrv")
#'
#' abbreviator("PGS", abbrv)
#'
#' }
#'
#' @export
abbreviator <- function(x, y) paste0("<a href='#' class='text-info' data-toggle='tooltip' title='", y[x]$def, "'>", x, "</a>")
