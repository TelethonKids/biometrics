#' @title Fill scale constructor for The Kids colors.
#'
#' @param palette Character name of palette in tehkids_palettes. Options are "primary", "tint50" and "typography"
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export
scale_fill_thekids <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- thekids_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("thekids_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
