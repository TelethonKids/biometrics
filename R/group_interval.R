#' Group by date interval
#'
#' Group records in a data frame by date that intervals including a buffer period.
#'
#' @param dat the data frame to be grouped, must be sorted by start date column
#' @param buffer the buffer interval e.g. days(14)
#' @param start,end the column name (as a character) containing the records'
#'   start and end dates, respectively
#' @param .view_mat when TRUE (defult FALSE) creates an overlap matrix and applies
#'   View()
#' @param .view_graph when TRUE (default FALSE) plots the graph of crouped records
#'
#' @return an integer vector of group numbers
#'
#' @import dplyr
#' @importFrom igraph components graph_from_data_frame
#' @importFrom lubridate interval
#' @importFrom tidyr spread
#'
#' @examples
#'
#' \dontrun{
#'
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
#'
#' }
#'
#' @export
group_interval <- function(dat, buffer, start, end, .view_mat = F, .view_graph = F) {

  dat[is.na(dat[[start]]) & is.na(dat[[end]]), ] <- NULL # remove row if both adm and sep is NA
  dat[is.na(dat[[start]]), start] <- dat[is.na(dat[[start]]), end] # set adm == sepa if missing adm
  dat[is.na(dat[[end]]), end] <- dat[is.na(dat[[end]]), start] # set sepa == adm if missing sepa
  dat[["interval_lower"]] <- dat[[start]] - buffer # start interval is y before adm date
  dat[["interval_upper"]] <- dat[[end]] + buffer # end interval is y after adm date

  dat <- dat %>%
    mutate(rid = 1:nrow(dat),# add in row id
           intervals = interval(interval_lower, interval_upper)) # define admissions as intervals incorporating buffer

  df_overlap <- bind_cols(
    expand.grid(dat$rid, dat$rid), # make a 2 col table with every combination of id numbers
    expand.grid(dat$intervals, dat$intervals)) %>% # make a combination of every interval
    mutate(overlap = int_overlaps(Var11, Var21)) %>% # determine if intervals overlap
    rename("row" = "Var1", "col" = "Var2")

  # Find groups via graph theory See igraph package
  dat_graph <- graph_from_data_frame(df_overlap[df_overlap[["overlap"]], c("row", "col")])
  groups <- components(dat_graph)$membership[df_overlap$row]

  # create a 2 column df with row (index) and group number, arrange on row number and return distinct values
  df_groups <- data_frame(row = as.integer(names(groups)), group = groups) %>%
    arrange(row) %>%
    distinct()

  # View df
  if (.view_mat) {
    df_view <- df_overlap %>%
      select(row, col, overlap) %>%
      spread(key = col, value = overlap) %>% # turn into a n x n data_frame
      select(-row)
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
  df_groups$group

}
