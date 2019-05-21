#' Telethon Kids Colour palettes per the 2014 Brand Style Guide.
library(biometrics)
telethonkids_palettes <- list(
  `light` = telethonkids_cols("PMS122", "PMS144", "PMS205", "PMS2395", "PMS7676", "PMS284", "PMS326", "PMS368", "PMS380"),
  `dark` = telethonkids_cols("PMS143", "PMS159", "PMS7425", "PMS513", "PMS7672", "PMS7683", "PMS3282", "PMS370", "PMS397"),
  `grey` = telethonkids_cols("Cool Grey 5", "Cool Grey 7", "Cool Grey 9", "Cool Grey 11")
)

#### save ----

devtools::use_data(telethonkids_palettes, overwrite = T)
