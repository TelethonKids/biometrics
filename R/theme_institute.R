#' Apply Institute Theme to ggplot2 Plots
#'
#' This function applies a custom theme to ggplot2 plots, incorporating specific fonts and colors to align with the institute's visual identity.
#'
#' @details The function determines the operating system and selects appropriate font names for Windows or other systems. It applies a minimal theme with custom settings for plot title, axis title, and strip text, using the 'Barlow Semi Condensed' font family. It also adjusts color scales using the 'viridis' package.
#'
#' @return A list of ggplot2 theme elements and scale adjustments.
#'
#' @examples
#' \dontrun{
#' # Install the required fonts first (see below)
#' # Example usage with ggplot2
#' library(ggplot2)
#' library(viridis)
#'
#' p <- ggplot(mtcars, aes(x = mpg, y = wt, fill = factor(cyl))) +
#'   geom_point() +
#'   theme_institute()
#'
#' print(p)
#' }
#'
#' @note To use this theme, you need to have the 'Barlow Semi Condensed' font family installed on your system.
#'
#' @section Installing Fonts:
#' To install the 'Barlow Semi Condensed' font family:
#'
#' 1. **Windows**:
#'    - Download the fonts from [Google Fonts](https://fonts.google.com/specimen/Barlow+Semi+Condensed).
#'    - Install the following font files:
#'      - `BarlowSemiCondensed-ExtraBold.ttf`
#'      - `BarlowSemiCondensed-Medium.ttf`
#'    - Using the extrafont package, run font_import() followed by loadfonts(device = "win")
#'
#' 2. **Mac OS and Linux**:
#'    - Download the fonts from [Google Fonts](https://fonts.google.com/specimen/Barlow+Semi+Condensed).
#'    - Install the following font files:
#'      - `BarlowSemiCondensed-Bold.ttf`
#'      - `BarlowSemiCondensed-Medium.ttf`
#'
#' @export
theme_institute <- function(base_size = 11, base_family = "Barlow Semi Condensed",
                            base_line_size = base_size/22, base_rect_size = base_size/22) {
  os <- Sys.info()[["sysname"]]

  font1 <- ifelse(os == "Windows",
                  "Barlow Semi Condensed ExtraBold",
                  "BarlowSemiCondensed-Bold")

  font2 <- ifelse(os == "Windows",
                  "Barlow Semi Condensed Medium",
                  "BarlowSemiCondensed-Medium")

  list(
    theme_minimal(base_family = "Barlow Semi Condensed",
                  base_size = base_size,
                  base_line_size = base_line_size,
                  base_rect_size = base_rect_size) +
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
