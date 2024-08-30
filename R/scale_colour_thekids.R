#' @title Color scale constructor for The Kids colours.
#'
#' @param palette Character name of palette in thekids_palettes. Options are "primary", "tint50" and "typography"
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @import ggplot2
#'
#' @export
scale_color_thekids <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- thekids_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("thekids_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}
