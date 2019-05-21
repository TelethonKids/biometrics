#' Excel column reference map
#'
#' A data table of alpha/numeric key/value pairs, where key is the
#' letter sequence of the excel column. Range of letter sequence is A - ZZZ.
#'
#' @docType data
#'
#' @usage data(excel_ref)
#'
#' @format A data table with 18278 rows and 2 columns:
#' \describe{
#'   \item{\code{letters}}{character Excel column reference letters}
#'   \item{\code{numbers}}{integer corresponding column number}
#' }
#'
#' @examples
#' \donttest{
#'   excel_ref["ABC"]$numbers
#' }
"excel_ref"

#' ICD-10 hash table
#'
#' A data.table that maps ICD-10 classification code to the disease
#' name, disease category, parent category and disease group. Codes were
#' sourced from GNU Health (downloaded June-2018).
#'
#' @docType data
#'
#' @usage data(ICD10)
#'
#' @format A data frame with 6820 rows and 5 variables:
#' \describe{
#'   \item{\code{id}}{character key ICD-10 codes WITHOUT decimal points}
#'   \item{\code{name}}{character disease name}
#'   \item{\code{disease_category}}{character disease category}
#'   \item{\code{parent_category}}{character parent disease category}
#'   \item{\code{disease_group}}{character Infections/notifiable}
#'}
#'
#' @source \url{http://health.gnu.org/}
"ICD10"

#' ICD-10-AM hash table
#'
#' A data.table that maps ICD-10-AM classification code to the
#' disease name, disease category, parent category and disease group. ICD-10
#' codes were sourced from GNU Health (downloaded June 2018) and mappings to
#' ICD-10-AM were sourced from the Australian Consortium for Classification
#' Development (downloaded 8-June-2018).
#'
#' @docType data
#'
#' @usage data(ICD10AM)
#'
#' @format A data frame with 10680 rows and 7 variables:
#' \describe{
#'   \item{\code{id}}{character key ICD-10-AM codes WITHOUT decimal points}
#'   \item{\code{name}}{character disease name}
#'   \item{\code{disease_category}}{character disease category}
#'   \item{\code{parent_category}}{character parent disease category}
#'   \item{\code{disease_group}}{character Infections/notifiable}
#'   \item{\code{record_type}}{character record type ICD-10/ICD-10-AM}
#'   \item{\code{mapping}}{character ICD-10 equivalent to the ICD-10-AM code}
#'}
#' @source \url{https://www.accd.net.au/Downloads.aspx}
"ICD10AM"

#' Telethon Kids Institute colours
#'
#' Hex codes for the Telethon Kids colours per the 2014 Brand Style Guide.
#'
#' @docType data
#'
#' @usage data(telethonkids_colours)
#'
#' @format A named vector key = pantone matching system color, value = hex colour
#'   code
"telethonkids_colours"

#' Telethon Kids Institute Palettes
#'
#' Telethon Kids colour palettes per the 2014 Brand Style Guide.
#'
#' @docType data
#'
#' @usage data(telethonkids_palettes)
#'
#' @format
#' \describe{
#'   \item{\code{light}}{Light Colour Tone Palette}
#'   \item{\code{dark}}{Dark Colour Tone Palette}
#'   \item{\code{grey}}{Typography Palette}
#' }
"telethonkids_palettes"
