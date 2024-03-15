#' js
#'
#' Helper function to include all inst/supporting_files/js files in yaml header.
#'
#' @export
js <- function() paste0(list.files(paste(system.file(package = "biometrics"), "supporting_files", "js", sep = "/"), full.names = TRUE), collapse = '", "')

#' html_report
#'
#' Remove pandoc configuration from .Rmd.
#'
#' @param toc default to TRUE to show menu bar
#' @param fold default to FALSE for collapsed code chunks
#'
#' @importFrom rmarkdown html_document
#'
#' @export
html_report <- function(toc = TRUE, fold = FALSE) {

  template <- system.file("supporting_files/html/html_report.html", package = "biometrics")
  css <- list.files(paste(system.file(package = "biometrics"), "supporting_files", "css", sep = "/"), full.names = TRUE)
  codefold = ifelse(fold, "hide", "show")

  html_document(fig_width = 6.5,
                fig_height = 4,
                toc = toc,
                toc_float = TRUE,
                self_contained = TRUE,
                template = template,
                css = css,
                code_folding = codefold)
}

#' html_ioslides
#'
#' Remove pandoc configuration from .Rmd.
#'
#' @importFrom rmarkdown ioslides_presentation
#'
#' @export
html_ioslides <- function() {

  template <- system.file("supporting_files/html/ioslides.html", package = "biometrics")
  css <- system.file("supporting_files/css/07_ioslides.css", package = "biometrics")
  logo <- system.file("supporting_files/images/logo800.jpg", package = "biometrics")

  ioslides_presentation(fig_width = 6.5,
                        fig_height = 4,
                        template = template,
                        css = css,
                        logo = logo,
                        widescreen = TRUE,
                        incremental = TRUE)
}

#' word
#'
#' Remove pandoc configuration from .Rmd.
#'
#' @importFrom rmarkdown word_document
#'
#' @export
word <- function() {

  template <- system.file("supporting_files/word/template.docx", package = "biometrics")
  pagebreak <- system.file("supporting_files/lua/pagebreak.lua", package = "biometrics")

  word_document(fig_width = 6.5,
                fig_height = 4,
                reference_docx = template,
                pandoc_args = paste0("--lua-filter=", pagebreak))
}
