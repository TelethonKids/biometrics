#' paste_clip
#'
#' Function to paste data from clipboard into R.
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
#'
#' @seealso \link[=paste_clip]{paste_clip()}
#'
#' @export
copy_clip <- function(x, row.names = FALSE, col.names = TRUE, ...) {
  write.table(x, "clipboard", sep = "\t", row.names = row.names, col.names = col.names, ...)
}
