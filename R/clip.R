#' paste_clip
#'
#' Function to paste data from clipboard into R.
#'
#' @param header default TRUE
#' @param ... other arguements passed to read.table
#'
#' @importFrom utils read.table
#'
#' @seealso \link[=copy_clip]{copy_clip()}
#'
#' @export
paste_clip <- function(header = TRUE, ...) {
  read.table("clipboard", sep = "\t", header = header, ...)
}

#' copy_clip
#'
#' Function to copy data from R to the clipboard.
#'
#' @param x object to be copied
#' @param row.names default FALSE
#' @param col.names default TRUE
#' @param ... other arguements passed to write.table
#'
#' @importFrom utils write.table
#'
#' @seealso \link[=paste_clip]{paste_clip()}
#'
#' @export
copy_clip <- function(x, row.names = FALSE, col.names = TRUE, ...) {
  write.table(x, "clipboard", sep = "\t", row.names = row.names, col.names = col.names, ...)
}
