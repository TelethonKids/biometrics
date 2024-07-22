#' theme_institute
#
#' Applies themes and colour schemes for universal ggplot2 formatting within the
#' biostatistics team. Barlow Semi Condensed Font Family can be downloaded
#' through Google Fonts
theme_institute <- function() {
  os <- Sys.info()[["sysname"]]

  font1 <- ifelse(os == "Windows",
                  "Barlow Semi Condensed ExtraBold",
                  "BarlowSemiCondensed-Bold")

  font2 <- ifelse(os == "Windows",
                  "Barlow Semi Condensed Medium",
                  "BarlowSemiCondensed-Medium")

  list(
    theme_minimal(base_family = "Barlow Semi Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(
            family = font1
            ),
          axis.title = element_text(
            family = font2
            ),
          strip.text = element_text(
            family = font1,
            size = rel(1), hjust = 0
          ),
          strip.background = element_rect(fill = "grey80", color = NA)),
    scale_colour_viridis_d(option = "plasma", end = 0.85),
    scale_fill_viridis_d(option = "plasma", end = 0.85)
  )
}
