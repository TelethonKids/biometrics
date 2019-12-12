#' Group by date interval
#'
#' Group records in a data frame by date that intervals including a buffer period.
#'

#' @param start,end vectors of the start and end dates/times
#' @param buffer the buffer interval e.g. days(14), defaults to 0
#' @param .view_mat when TRUE (defult FALSE) creates an overlap matrix and applies
#'   View()
#' @param .view_graph when TRUE (default FALSE) plots the graph of crouped records
#'
#' @return an integer vector of group numbers
#'
#' @import dplyr
#' @importFrom igraph components graph_from_data_frame layout_with_fr
#' @importFrom lubridate interval int_overlaps int_start int_end seconds
#' @importFrom tidyr spread
#' @importFrom utils View
#' @importFrom graphics plot
#'
#' @examples
#'
#' \dontrun{
#'
#' dat <- tibble(
#'   name = c("Paul", "Jess", "Ben", "Josie", "Damien", "Hayley"),
#'   start = ymd(c("2018-05-25", "2018-05-29", NA, "2018-06-25",
#'     "2019-07-15", "2019-07-08")),
#'   end = ymd(c("2018-05-30", "2018-06-03", "2018-06-23",
#'     "2018-06-29", "2019-07-18", "2019-07-13")))
#'
#' group_interval(dat$start, dat$end, days(2))
#'
#' mutate(dat, group = group_interval(start, end, days(2)))
#'
#' }
#'
#' @export
group_interval <- function(start, end, buffer = 0, .view_mat = F, .view_graph = F) {

  overlap <- rid <- int_start <- int_end <-  NULL

  dat <- tibble(rid = 1:length(start),
                start = start,
                end = end,
                intervals = case_when(!is.na(start) & !is.na(end) ~ interval(start, end),
                                      is.na(start) ~ interval(end, end),
                                      is.na(end) ~ interval(start, start),
                                      TRUE ~ interval(NA, NA)))

  # apply buffer period to intervals
  lubridate::int_start(dat$intervals) <- lubridate::int_start(dat$intervals) - buffer + seconds(0.01)
  lubridate::int_end(dat$intervals) <- lubridate::int_end(dat$intervals) + buffer - seconds(0.01)

  # make intervals from all stat/end combinations
  df_overlap <- bind_cols(
    expand.grid(dat$rid, dat$rid), # make a 2 col table with every combination of id numbers
    expand.grid(dat$intervals, dat$intervals)) %>% # make a combination of every interval
    mutate(overlap = int_overlaps(.data$Var11, .data$Var21)) %>% # determine if intervals overlap
    rename("row" = "Var1", "col" = "Var2")

  # Find groups via graph theory See igraph package
  dat_graph <- graph_from_data_frame(filter(df_overlap, overlap) %>% select(row, col))
  groups <- components(dat_graph)$membership[df_overlap$row]

  # create a 2 column df with row (index) and group number, arrange on row number and return distinct values
  df_groups <- tibble(row = as.integer(names(groups)), group = groups) %>%
    unique()

  # View df
  if (.view_mat) {
    df_view <- df_overlap %>%
      select(.data$row, .data$col, .data$overlap) %>%
      spread(key = .data$col, value = .data$overlap) %>% # turn into a n x n tibble
      select(-.data$row)
    df_view[lower.tri(df_view, diag = F)] <- F # set lower triangle to FALSE (keep diagonals as TRUE)
    apply(df_view, 2, function(x) ifelse(!x, NA, x)) %>% # change FALSE to NA
      View()
  }

  # View graph
  if (.view_graph) {
    plot(dat_graph, layout = layout_with_fr, vertex.size = 4,
         vertex.label.dist = 0.5, vertex.color = "red", edge.arrow.size = 0.5)
  }

  # returns
  left_join(select(dat, rid), df_groups, by = c("rid" = "row"))$group

}
