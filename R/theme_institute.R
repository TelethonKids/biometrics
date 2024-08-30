#' Apply Institute Theme to ggplot2 Plots
#'
#' This function applies a custom theme to ggplot2 plots, incorporating specific fonts and colors to align with the institute's visual identity.
#'
#' @details The function determines the operating system and selects appropriate font names for Windows or other systems. It applies a minimal theme with custom settings for plot title, axis title, and strip text, using the 'Barlow Semi Condensed' font family. It also adjusts color scales using the 'viridis' package.
#'
#' @param base_size The base font size, given in points. Default is 11.
#' @param base_family The base font family used for the text. Default is `"Barlow Semi Condensed"`.
#' @param base_line_size The base size for line elements (e.g., axis lines, grid lines). Calculated as `base_size/22` by default.
#' @param base_rect_size The base size for rect elements (e.g., plot background, legend keys). Calculated as `base_size/22` by default.
#' @param scale_colour_type Type of scale used for colours. Should be either `"discrete"` or `"continuous"`. Default is `"discrete"`.
#' @param scale_fill_type Type of scale used for fills. Should be either `"discrete"` or `"continuous"`. Default is `"discrete"`.
#' @param colour_theme Colour palette to use for colour scales. Must be one of `"viridis"`,`"thekids"`,`"thekids_tint"`,`"thekids_grey"`. Default is `"viridis"`.
#' @param fill_theme Colour palette to use for fill scales. Must be one of `"viridis"`,`"thekids"`,`"thekids_tint"`,`"thekids_grey"`. Default is `"viridis"`.
#' @param rev_colour Logical. Should the colour palette be reversed? Default is `FALSE`.
#' @param rev_fill Logical. Should the fill palette be reversed? Default is `FALSE`.
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
#' p <- ggplot(mtcars, aes(x = mpg, y = wt, col = factor(cyl))) +
#'   geom_point() +
#'   theme_institute()
#'
#' print(p)
#'
#' p2 <- gplot(mtcars, aes(x = factor(cyl), y = wt, fill = factor(cyl))) +
#'   geom_col() +
#'   theme_institute(fill_theme = "thekids_tint", rev_fill = T)
#'
#' print(p2)
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
                            base_line_size = base_size/22, base_rect_size = base_size/22,
                            scale_colour_type = "discrete", scale_fill_type = "discrete",
                            colour_theme = "viridis", fill_theme = "viridis",
                            rev_colour = F, rev_fill = F) {
  os <- Sys.info()[["sysname"]]

  font1 <- ifelse(os == "Windows",
                  "Barlow Semi Condensed ExtraBold",
                  "BarlowSemiCondensed-Bold")

  font2 <- ifelse(os == "Windows",
                  "Barlow Semi Condensed Medium",
                  "BarlowSemiCondensed-Medium")

  colour_function <- case_when(
    colour_theme == "viridis" & scale_colour_type == "discrete" ~
      list(scale_colour_viridis_d(option = "plasma", end = 0.85)),
    colour_theme == "viridis" & scale_colour_type == "continuous" ~
      list(scale_colour_viridis_c(option = "plasma", end = 0.85)),
    colour_theme == "thekids" ~
      list(scale_color_thekids(palette = "primary", reverse = rev_colour)),
    colour_theme == "thekids_tint" ~
      list(scale_color_thekids(palette = "tint50", reverse = rev_colour)),
    colour_theme == "thekids_grey" ~
      list(scale_color_thekids(palette = "typography", reverse = rev_colour)),
  )[[1]]

  fill_function <- case_when(
    fill_theme == "viridis" & scale_fill_type == "discrete" ~
      list(scale_fill_viridis_d(option = "plasma", end = 0.85)),
    fill_theme == "viridis" & scale_fill_type == "continuous" ~
      list(scale_fill_viridis_c(option = "plasma", end = 0.85)),
    fill_theme == "thekids" ~
      list(scale_fill_thekids(palette = "primary", reverse = rev_fill)),
    fill_theme == "thekids_tint" ~
      list(scale_fill_thekids(palette = "tint50", reverse = rev_fill)),
    fill_theme == "thekids_grey" ~
      list(scale_fill_thekids(palette = "typography", reverse = rev_fill)),
  )[[1]]

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
          plot.background = element_rect(fill = "white", colour = "white"),
          strip.background = element_rect(fill = "grey80", colour = NA)),
    colour_function,
    fill_function
  )
}
