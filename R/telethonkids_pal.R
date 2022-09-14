#' @title Return function to interpolate a Telethon Kids color palette
#'
#' @param palette Character name of palette in telethonkids_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @importFrom grDevices colorRampPalette
#'
#' @export
telethonkids_pal <- function(palette = "light", reverse = FALSE, ...) {

  pal <- biometrics::telethonkids_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)

}
