#' Themed table output
#'
#' Using flextable (as this originated with a focus on word documents), this applies theming to table output in line with The Kids branding
#'
#' @param start,end vectors of the start and end dates/times
#' @param buffer the buffer interval e.g. days(14), defaults to 0
#' @param .view_mat when TRUE (defult FALSE) creates an overlap matrix and applies
#'   View()
#' @param .view_graph when TRUE (default FALSE) plots the graph of crouped records
#'
#' @return an integer vector of group numbers
#'
#' @import tidyverse flextable
#'
#' @examples
#'
#' \dontrun{
#'
#' head(mtcars, 10) %>%
#'   thekids_table(colour = "Saffron")
#' }
#'
#' @export
#'

thekids_table <- function(x,
                            font.size = 8,
                            font.size.header = 10,
                            line.spacing = 1.5,
                            padding = 2,
                            colour = "CoolGrey"){

  if(!colour %in% names(thekids_palettes$primary)){
    stop("The colour you have provided is not in the list. Please select from: Saffron, Pumpkin, Teal, DarkTeal, CelestialBlue, AzurBlue, MidnightBlue, or CoolGrey")
  }

  theme_thekids_zebra <- function (x,
                                   odd_header = thekids_palettes$primary[[paste(colour)]],
                                   odd_body = thekids_palettes$tint50[[paste(colour)]],
                                   even_header = "transparent",
                                   even_body = "transparent")
  {
    if (!inherits(x, "flextable")) {
      stop(sprintf("Function `%s` supports only flextable objects.",
                   "theme_kids_zebra()"))
    }
    h_nrow <- nrow_part(x, "header")
    f_nrow <- nrow_part(x, "footer")
    b_nrow <- nrow_part(x, "body")
    x <- border_remove(x)
    x <- align(x = x, align = "center", part = "header")
    if (h_nrow > 0) {
      even <- seq_len(h_nrow)%%2 == 0
      odd <- !even
      x <- bg(x = x, i = odd, bg = odd_header, part = "header")
      x <- bg(x = x, i = even, bg = even_header, part = "header")
      x <- bold(x = x, bold = TRUE, part = "header")
    }
    if (f_nrow > 0) {
      even <- seq_len(f_nrow)%%2 == 0
      odd <- !even
      x <- bg(x = x, i = odd, bg = odd_header, part = "footer")
      x <- bg(x = x, i = even, bg = even_header, part = "footer")
      x <- bold(x = x, bold = TRUE, part = "footer")
    }
    if (b_nrow > 0) {
      even <- seq_len(b_nrow)%%2 == 0
      odd <- !even
      x <- bg(x = x, i = odd, bg = odd_body, part = "body")
      x <- bg(x = x, i = even, bg = even_body, part = "body")
    }
    x <- align_text_col(x, align = "left", header = TRUE)
    x <- align_nottext_col(x, align = "right", header = TRUE)
    x
  }

  set_flextable_defaults(
    font.family = "Barlow",
    font.size = font.size,
    theme_fun = theme_thekids_zebra,
    line_spacing = line.spacing,
    padding = padding,
    big.mark="",
    table.layout="autofit")

  x %>%
    flextable() %>%
    fontsize(part = "header", size = font.size.header) %>%
    color(color = "white", part = "header") %>%
    color(color = "#111921", part = "body") %>%
    autofit()
}
