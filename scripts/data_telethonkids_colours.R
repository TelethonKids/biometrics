#' Telethon Kids Institute colours per the 2014 Brand Style Guide.
#'
#' Created by Paul Stevenson 21-May-2019.

telethonkids_colours <- c(
  #  Light colour tone palette
  `PMS122` = "#FED141",
  `PMS144` = "#ED8B00",
  `PMS205` = "#E0457B",
  `PMS2395` = "#C800A1",
  `PMS7676` = "#7566A0",
  `PMS284` = "#6CACE4",
  `PMS326` = "#00B2A9",
  `PMS368` = "#78BE20",
  `PMS380` = "#C4D6A4",

  # Dark colour tone palette
  `PMS143` = "#F1B434",
  `PMS159` = "#CB6015",
  `PMS7425` = "#B52555",
  `PMS513` = "#93328E",
  `PMS7672` = "#4C4184",
  `PMS7683` = "#426DA9",
  `PMS3282` = "#008578",
  `PMS370` = "#658D1B",
  `PMS397` = "#BFB800",

  # Typography palette
  `Cool Grey 5` = "#B1B3B3",
  `Cool Grey 7` = "#97999B",
  `Cool Grey 9` = "#75787B",
  `Cool Grey 11` = "#53565A"
)

#### save ----

devtools::use_data(telethonkids_colours, overwrite = T)
