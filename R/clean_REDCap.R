# clean_REDCap.R
# Helper functions to clean imported REDCap data per the REDCap data dictionary
# Cretaed by Paul Stevenson, 2021-05-14

#' factor_table
#'
#' @param x character vector of REDCap options (e.g. 0, No | 1, Yes)
#' @param rows (default "\\|") Regex defining row separator
#' @param cols (default ",") Regex defining column separator
#'
#' @return 2 column tibble of factor levels ("key") and labels ("value")
#'
#' @importFrom tibble tibble
#' @importFrom stringr str_split
#' @importFrom tidyr separate
#' @importFrom dplyr mutate across
#' @importFrom tidyselect everything
#'
#' @examples
#' \dontrun{ factor_table("0, No | 1, Yes") }
#'
#' @export
factor_table <- function(x, rows = "\\|", cols = ",") {

  key <- NULL

  tibble(key = str_split(x, rows)[[1]]) %>%
  separate(key, into = c("key", "value"), sep = cols, extra = "merge") %>%
  mutate(across(everything(), trimws))

}

#' factor_convert
#'
#' Apply factor labels to categorical responses per REDCap data dictionary.
#'
#' @param x character vector
#' @param d REDCap import
#' @param dict REDCap data dictionary
#'
#' @importFrom dplyr cur_column
#'
#' @examples
#' \dontrun{
#'
#' dat <- tibble(var = c("0", "1"))
#'
#' dictionary <- tibble(`Variable / Field Name` = "var",
#'   `Choices, Calculations, OR Slider Labels` = "0, No | 1, Yes")
#'
#' mutate(dat, across("var", factor_convert, d = dat, dict = dictionary))
#'
#' }
#'
#' @export
factor_convert <- function(x, d, dict) {
  y <- factor_table(dict[dict$`Variable / Field Name` == cur_column(),]$`Choices, Calculations, OR Slider Labels`)
  factor(x, levels = y$key, labels = y$value)
}

#' checkbox_labels
#'
#' Apply factor labels to categorical responses (checkboxes) per REDCap data dictionary
#'
#' @param x checkbox `Variable / Field Name` per data dictionary
#' @param dict REDCap data dictionary
#'
#' @return Named list of character objects
#'
#' @importFrom dplyr cur_column
#'
#' @examples
#' \dontrun{
#'
#' dat <- tibble(var___0 = c("0", "1"), var___1 = c("1", "0"))
#'
#' dictionary <- tibble(`Variable / Field Name` = "var",
#'   `Choices, Calculations, OR Slider Labels` = "0, No | 1, Yes")
#'
#' dat <- labelled::set_variable_labels(dat, .labels = checkbox_labels("var", dictionary))
#'
#' }
#'
#' @export
checkbox_labels <- function(x, dict) {

  d <- factor_table(dict[dict$`Variable / Field Name` == x,]$`Choices, Calculations, OR Slider Labels`) %>%
    mutate(across("key", ~paste(x, ., sep = "___")))

  o <- as.list(d$value)
  names(o) <- d$key

  o
}

#' variable_labels
#'
#' Apply variable labels to a data frame per the REDCap data dictionary. If
#' there is no variable label associated with a column then is lable will be the
#' same as the column's name.
#'
#' @param d REDCap import
#' @param dict REDCap data dictionary
#'
#' @return character vector (length `ncol(d)`) of REDCap Field Labels per the data dictionary
#'
#' @importFrom purrr map_chr
#'
#' @examples
#' \dontrun{
#'
#' dat <- tibble(var = 1)
#'
#' dictionary <- tibble(`Variable / Field Name` = "var", `Field Label` = "Label")
#'
#' dat <- labelled::set_variable_labels(dat, .labels = variable_labels(dat, dictionary))
#'
#' }
#'
#' @export
variable_labels <- function(d, dict) map_chr(names(d),
                                             ~ifelse(.x %in% dict$`Variable / Field Name`,
                                                     dict[dict$`Variable / Field Name` == .x,]$`Field Label`,
                                                     .x), dict = dict)

#' clean_REDCap
#'
#' Clean a REDCap extract by applyly factor levels and convert column classes
#' per the REDCap data dictionary.
#'
#' @param d REDCap (data frame)
#' @param dict REDCap data dictionary (data frame)
#' @param numeric_date (default FALSE) set to TRUE if MS Excel has _helpfully_ converted to a numeric date
#'
#' @return cleaned data frame
#'
#' @importFrom stringr str_replace
#' @importFrom dplyr mutate across
#' @importFrom magrittr `%>%`
#' @importFrom tidyselect starts_with
#' @importFrom purrr map_lgl
#' @importFrom labelled set_variable_labels
#' @importFrom lubridate dmy mdy ymd dmy_hm mdy_hm ymd_hm dmy_hms mdy_hms ymd_hms
#' @importFrom janitor excel_numeric_to_date
#'
#' @examples
#' \dontrun{
#'
#' dat <- tibble(var = c("0", "1"))
#'
#' dictionary <- tibble(`Variable / Field Name` = "var",
#'   `Field Type` = "radio",
#'   `Field Label` = "Label",
#'   `Choices, Calculations, OR Slider Labels` = "0, No | 1, Yes",
#'   `Text Validation Type OR Show Slider Number` = NA)
#'
#' (dat_clean <- clean_REDCap(dat, dictionary))
#'
#' }
#'
#' @export
clean_REDCap <- function(d, dict, numeric_date = FALSE) {

  dict <- dict[dict$`Variable / Field Name` %in% str_replace(names(d), "___\\d+", ""),] # remove items from dictionary that aren't in the dataset

  d <- mutate(d,
              across(dict[map_lgl(dict$`Field Type` == "yesno", isTRUE),]$`Variable / Field Name`, ~as.logical(as.numeric(.x))),
              across(dict[map_lgl(dict$`Field Type` == "calc", isTRUE),]$`Variable / Field Name`, as.numeric),
              across(starts_with(paste0(dict[map_lgl(dict$`Field Type` == "checkbox", isTRUE),]$`Variable / Field Name`, "___")), ~as.logical(as.numeric(.))),
              across(dict[map_lgl(dict$`Field Type` %in% c("dropdown", "radio"), isTRUE),]$`Variable / Field Name`, factor_convert, d = d, dict = dict),

              across(dict[map_lgl(dict$`Text Validation Type OR Show Slider Number` == "integer", isTRUE),]$`Variable / Field Name`, as.integer),
              across(dict[grepl("^number((?!comma).)*$", dict$`Text Validation Type OR Show Slider Number`, perl = TRUE),]$`Variable / Field Name`, as.numeric), # starts with "number", does not contain "comma"
              across(dict[grepl("^number.*comma.*$", dict$`Text Validation Type OR Show Slider Number`),]$`Variable / Field Name`, ~as.numeric(sub(",", ".", .x)))) # starts with "number", does contain "comma"

  if (numeric_date) {
    d <- mutate(d, across(dict[grepl("^date.*$", dict$`Text Validation Type OR Show Slider Number`, perl = TRUE),]$`Variable / Field Name`, ~excel_numeric_to_date(as.numeric(.))))
  } else {
    d <- mutate(d,
                across(dict[map_lgl(dict$`Text Validation Type OR Show Slider Number` == "date_dmy", isTRUE),]$`Variable / Field Name`, dmy),
                across(dict[map_lgl(dict$`Text Validation Type OR Show Slider Number` == "date_mdy", isTRUE),]$`Variable / Field Name`, mdy),
                across(dict[map_lgl(dict$`Text Validation Type OR Show Slider Number` == "date_ymd", isTRUE),]$`Variable / Field Name`, ymd),
                across(dict[map_lgl(dict$`Text Validation Type OR Show Slider Number` == "datetime_dmy", isTRUE),]$`Variable / Field Name`, dmy_hm),
                across(dict[map_lgl(dict$`Text Validation Type OR Show Slider Number` == "datetime_mdy", isTRUE),]$`Variable / Field Name`, mdy_hm),
                across(dict[map_lgl(dict$`Text Validation Type OR Show Slider Number` == "datetime_ymd", isTRUE),]$`Variable / Field Name`, ymd_hm),
                across(dict[map_lgl(dict$`Text Validation Type OR Show Slider Number` == "datetime_seconds_dmy", isTRUE),]$`Variable / Field Name`, dmy_hms),
                across(dict[map_lgl(dict$`Text Validation Type OR Show Slider Number` == "datetime_seconds_mdy", isTRUE),]$`Variable / Field Name`, mdy_hms),
                across(dict[map_lgl(dict$`Text Validation Type OR Show Slider Number` == "datetime_seconds_ymd", isTRUE),]$`Variable / Field Name`, ymd_hms))
  }

  d <- set_variable_labels(d, .labels = variable_labels(d, dict))

  # Apply checkbox labels

  for (i in dict[map_lgl(dict$`Field Type` == "checkbox", isTRUE),]$`Variable / Field Name`) {
    d <- set_variable_labels(d, .labels = checkbox_labels(i, dict))
  }

  d
}
