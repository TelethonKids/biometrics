#' @title Fill scale constructor for Telethon Kids colors.
#'
#' @param palette Character name of palette in telethonkids_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
scale_fill_telethonkids <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- telethonkids_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("telethonkids_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
