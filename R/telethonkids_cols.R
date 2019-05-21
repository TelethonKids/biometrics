#' @title Function to extract Telethon Kids colors as hex codes
#'
#' @param ... Character names of telethonkids_colours
#'
#' @export
telethonkids_cols <- function(...) {

  cols <- c(...)

  if (is.null(cols))
    return (biometrics::telethonkids_colours)

  biometrics::telethonkids_colours[cols]

}
