#' @title Color scale constructor for Telethon Kids colours.
#'
#' @param palette Character name of palette in telethonkids_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @import ggplot2
#' @export
scale_color_telethonkids <- function(palette = "light", discrete = TRUE, reverse = FALSE, ...) {
  pal <- telethonkids_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("telethonkids_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}
