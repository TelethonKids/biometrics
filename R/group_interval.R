#' Group by Interval
#'
#' Group records in a data frame by date intervals and proximity
#'
#' @param x the data frame to be grouped, must be sorted by start date column
#' @param b the buffer interval i.e . /{lubridate::days(14)}
#' @param start,end the column name (as a character) containing the records'
#'   start and end dates, respectively
#' @param debug passing debug = T (default is F) will mutate the input data frame
#'   with the group number and intermediate calculated value and write to the
#'   global variable group_interval_checking (not active)
#'
#' @return an integer vector of group numbers
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import tidyr
#' @importFrom data.table shift
#' @importFrom lubridate interval as.duration
#'
#' @examples
#' \dontrun{
#' dat <- data_frame(
#'   name = c("Paul", "Jess", "Ben", "Josie", "Damien", "Hayley"),
#'   start = ymd(c("2018-05-25", "2018-05-29", NA, "2018-06-25",
#'     "2019-07-15", "2019-07-08")),
#'   end = ymd(c("2018-05-30", "2018-06-03", "2018-06-23",
#'     "2018-06-29", "2019-07-18", "2019-07-13")))
#'
#' dat <- arrange(dat, start, end)
#'
#' group_interval(dat, days(2), "start", "end")
#' }
#'
#' @export
group_interval <- function(x, b, start, end, debug = F) {
  # Offset start date and
  x[[end]] <- apply(x, 1, function(y) ifelse(is.na(y[[end]]), y[[start]], y[[end]]))
  next_adm <- shift(x[[start]], n = 1L, fill = NA, type = "lead")

  # calculate interval between records
  record_interval = interval(x[[end]], next_adm) %>%
    as.duration() %>% as.numeric("seconds")

  # put everything in data frame
  d <- data_frame(record_interval = record_interval, buffer = b %>%
                    as.duration() %>% as.numeric("seconds"))

  # determine if interval is greater than allowable difference
  o <- ifelse(d[["record_interval"]] <= d[["buffer"]], F, T)

  # determine group number(s)
  o[is.na(o)] <- T

  # apply group numbers
  output <- c(1, 1 + cumsum(o))[1:length(o)] %>% as.integer()

  # error checking
  d <- mutate(d,
              adm = x[[start]], sepa = x[[end]], next_adm = next_adm,
              interval_as_date = interval(x[[end]], next_adm) %>% as.duration(),
              same_next = o, group_no = output)

  # if (debug == T) group_interval_checking <- d[, c("adm", "sepa", "next_adm", "record_interval", "buffer", "interval_as_date", "same_next", "group_no")]

  d[["group_no"]]

}
