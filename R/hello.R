#' A function to generate a data dictionary for a REDCap dataset.
#'
#'
#' @param text some text
#'
#' @return text
#'
#' @examples
#' \dontrun{
#' questionnaire_dictionary <- exportMeta(redcap_connection)
#' create_REDCap_dictionary(questionnaire_year1, questionnaire_dictionary)
#' }
#'
#' @export

hello <- function(text) {
  paste("Hello", text)
}
