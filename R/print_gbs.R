#' print_gbs
#'
#' Print the output from a gb (group_by) s (summarise). The purpose of this function is to aid in making a common table, with groups as columns and summary statistics in the cells. When creating this via `group_by() %>% summarise()` you need to transpose the output and correct the column headings, this function does that.
#'
#' @param x the output, via piping, of a summarise statement that follows a group_by (tidyverse)
#' @param kable (default F) add kable and kable_styling to the output (for using within markdown documents)
#'
#' @return A table (either `tibble` or `kable` based)
#'
#' @import magrittr
#' @importFrom kableExtra kable_styling
#'
#' @examples
#' \dontrun{
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(cyl_n = n(),
#'             mean_hp = mean(hp)) %>%
#'   print_gbs()
#' }
#'

# Transpose the output of a group_by %>% summarise for table generation, with the option to kable the output
print_gbs <- function(x, kable = F) {
  x <- data.frame(t(x))
  names(x) <- x[1,]
  x <- x[-1, ]

  if(kable == F){
    return(x)
  } else{
    x %>%
      kable %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
      return
  }
}
