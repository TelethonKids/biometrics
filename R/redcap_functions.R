#' create_REDCap_dictionary
#'
#' A function to generate a data dictionary for a REDCap dataset.
#'
#'
#' @param x a dataframe with REDCap column names retained.
#' @param dictionary a REDCap dictionary (extracted from REDCap with standard formatting)
#'
#' @return object of class dataframe"
#'
#' @examples
#' \dontrun {
#' questionnaire_dictionary <- exportMeta(redcap_connection)
#' create_REDCap_dictionary(questionnaire_year1, questionnaire_dictionary)
#' }
#'
#' @export

create_REDCap_dictionary <- function(x, dictionary) {

  names <- unique(str_replace_all(colnames(x) , "___\\d+$", ""))

  # Filtered Dictionary
  output <- dictionary %>%
    filter(`Variable / Field Name` %in% names) %>%
    arrange(factor(`Variable / Field Name`, levels = names)) %>%
    mutate(across(everything(), ~ifelse(is.na(.), "", as.character(.))))

  output
}
