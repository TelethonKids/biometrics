#' data-raw/DATASET.R
#'
#' Create package data sets
#'

#### library ----

library(biometrics)
library(dplyr)
library(data.table)
library(labelled)
library(lubridate)
library(readr)
library(XML)

#### Excel reference ----

#' A data table of alpha/numeric key/value pairs, where key is the
#' letter sequence of the excel column. Range of letter sequence is A - ZZZ.


excel_ref <- bind_rows(

  tibble(letters = LETTERS),

  expand.grid(LETTERS, LETTERS, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(letters = paste0(Var2, Var1)) %>%
    select(letters),

  expand.grid(LETTERS, LETTERS, LETTERS, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(letters = paste0(Var3, Var2, Var1)) %>%
    select(letters)

) %>%
  mutate(numbers = 1:nrow(.)) %>%
  as.data.table()

setkey(excel_ref, letters)

#### ICD-10 data dictionary ----

# parse diseases
xmlParse("data-raw/diseases.xml") -> diseases_xml
xpathSApply(diseases_xml, "//record", xmlGetAttr, 'id') -> id
xpathSApply(diseases_xml, "//field[@name='name']", xmlValue) -> name
xpathSApply(diseases_xml, "//field[@name='category']", xmlGetAttr, 'ref') -> category
xpathSApply(diseases_xml, "//field[@name='active']", xmlValue) -> active
setDT(data.frame(id = id, name = name, category = category, active = active, stringsAsFactors = FALSE)) -> diseases_dt
setkey(diseases_dt, id)

# parse disease categories
xmlParse("data-raw/disease_categories.xml") -> categories_xml
xpathSApply(categories_xml, "//record", xmlGetAttr, 'id') -> id
xpathSApply(categories_xml, "//field[@name='name']", xmlValue) -> name
xpathSApply(categories_xml, "//field[@name='parent']", xmlGetAttr, 'ref') -> parent
c(rep(NA, length(id) - length(parent)), parent) -> parent
setDT(data.frame(id = id, category = name, parent = parent, stringsAsFactors = FALSE)) -> categories_dt
setkey(categories_dt, id)

# merge into one data table
merge(diseases_dt, categories_dt, by.x = "category", by.y = "id") -> dt
setkey(dt, id)
dt[, category := NULL]
"disease_category" -> colnames(dt)[colnames(dt) == "category.y"]
gsub("\\s*\\([^\\)]+\\)\\s+","", as.character(dt[["disease_category"]])) -> dt[["disease_category"]]
categories_dt[dt[["parent"]], category] -> dt[["parent"]]
"parent_category" -> colnames(dt)[colnames(dt) == "parent"]

# parse disease groups
xmlParse("data-raw/disease_groups.xml") -> groups_xml
xpathSApply(groups_xml, "//record") -> a
xpathSApply(groups_xml, "//record", xmlGetAttr, 'id') -> id
xpathSApply(groups_xml, "//field[@name='name']", xmlGetAttr, 'ref') -> name
xpathSApply(groups_xml, "//field[@name='disease_group']", xmlGetAttr, 'ref') -> disease_group
setDT(data.frame(id = id, name = name, disease_group = disease_group, stringsAsFactors = FALSE)) -> groups_dt
setkey(groups_dt, id)

# concatenate groups into a character vector
merge(dt, groups_dt, by.x = "id", by.y = "name") -> groups
dcast(groups, id ~ "id.y", value.var = "id.y", fun.aggregate = function(x) paste(x, collapse = ",")) -> groups_cat_dt
"disease_group" -> colnames(groups_cat_dt)[colnames(groups_cat_dt) == "."]

merge(dt, groups_cat_dt, by = "id") -> ICD10

ICD10[, "active"] <- NULL

remove(a, categories_dt, diseases_dt, dt, groups, groups_cat_dt, groups_dt,
       active, categories_xml, category, disease_group, diseases_xml, groups_xml,
       id, name, parent)

#### ICD-10-AM data dictionary ----

fread("data-raw/ICD10AM_mapping_tables.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE) -> ICD10AM

ICD10AM[, "Field4"] <- NULL

# remove full stops from code and mapping
gsub("\\.","", as.character(ICD10AM[["ICD-10-AM Code"]])) -> ICD10AM[["ICD-10-AM Code"]]
gsub("\\.","", as.character(ICD10AM[["ICD-10 Map"]])) -> ICD10AM[["ICD-10 Map"]]

# harmonise column names
"id" -> colnames(ICD10AM)[colnames(ICD10AM) == "ICD-10-AM Code"]
"name" -> colnames(ICD10AM)[colnames(ICD10AM) == "ICD-10-AM Code Descriptor"]
"mapping" -> colnames(ICD10AM)[colnames(ICD10AM) == "ICD-10 Map"]

# add database information
ICD10[, record_type := list(rep("ICD-10", dim(ICD10)[1]))]
ICD10AM[, record_type := list(rep("ICD-10-AM", dim(ICD10AM)[1]))]

# add disease_category, parent_category and disease_group to ICD-10-AM from ICD-10
ICD10AM[, disease_category := list(ICD10[ICD10AM[["mapping"]], disease_category])]
ICD10AM[, parent_category := list(ICD10[ICD10AM[["mapping"]], parent_category])]
ICD10AM[, disease_group := list(ICD10[ICD10AM[["mapping"]], disease_group])]

# remove conflicts
ICD10AM[!id %in% c("O10", "O94")] -> ICD10AM

# join datasets
rbind(ICD10, ICD10AM, fill = TRUE) -> ICD10AM
setkey(ICD10AM, id)

# remove non-ASCII characters
ICD10AM["C8800"][["name"]] <- "Waldenstrom macroglobulinaemia, without mention of remission"
ICD10AM["C8801"][["name"]] <- "Waldenstrom macroglobulinaemia, in remission"
ICD10AM["Y9253"][["name"]] <- "Trade and service area, cafe, hotel and restaurant"

#### Telethon Kids Institute colours per the 2014 Brand Style Guide ----

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

#### Telethon Kids Colour palettes per the 2014 Brand Style Guide ----

telethonkids_palettes <- list(
  `light` = telethonkids_cols("PMS122", "PMS144", "PMS205", "PMS2395", "PMS7676", "PMS284", "PMS326", "PMS368", "PMS380"),
  `dark` = telethonkids_cols("PMS143", "PMS159", "PMS7425", "PMS513", "PMS7672", "PMS7683", "PMS3282", "PMS370", "PMS397"),
  `grey` = telethonkids_cols("Cool Grey 5", "Cool Grey 7", "Cool Grey 9", "Cool Grey 11")
)

#### REDCap clean test data

clean_REDCap_dat <- read_csv(file = "data-raw/REDCap_test_data.csv", col_types = cols(.default = "c"))
clean_REDCap_dat_dictionary <- read_csv(file = "data-raw/REDCap_dictionary.csv", col_types = cols(.default = "c"))

clean_REDCap_validation <- tibble(id1 = as.character(1:2),
                                  yesno1 = c(FALSE, TRUE),
                                  checkbox1___1 = c(FALSE, TRUE),
                                  checkbox1___2 = c(TRUE, FALSE),
                                  dropdown1 = factor(c("Dropdown 1", "Dropdown 2"), levels = c("Dropdown 1", "Dropdown 2")),
                                  radio1 = factor(c("Radio 1", "Radio 2"), levels = c("Radio 1", "Radio 2")),
                                  date_dmy1 = rep(dmy("16/02/1985"), 2),
                                  date_mdy1 = rep(dmy("16/02/1985"), 2),
                                  date_ymd1 = rep(dmy("16/02/1985"), 2),
                                  datetime_dmy1 = c(dmy_hm("16/02/1985 12:01 AM"), dmy_hm("16/02/1985 12:01 PM")),
                                  datetime_mdy1 = c(dmy_hm("16/02/1985 12:01 AM"), dmy_hm("16/02/1985 12:01 PM")),
                                  datetime_ymd1 = c(dmy_hm("16/02/1985 12:01 AM"), dmy_hm("16/02/1985 12:01 PM")),
                                  datetime_seconds_dmy1 = c(dmy_hms("16/02/1985 12:01:01 AM"), dmy_hms("16/02/1985 12:01:01 PM")),
                                  datetime_seconds_mdy1 = c(dmy_hms("16/02/1985 12:01:01 AM"), dmy_hms("16/02/1985 12:01:01 PM")),
                                  datetime_seconds_ymd1 = c(dmy_hms("16/02/1985 12:01:01 AM"), dmy_hms("16/02/1985 12:01:01 PM")),
                                  integer1 = rep(1L, 2),
                                  number1 = rep(1.5, 2),
                                  number_comma_decimal1 = rep(1.5, 2),
                                  number_1dp1 = rep(1.5, 2),
                                  number_1dp_comma_decimal1 = rep(1.5, 2))
var_label(clean_REDCap_validation) <- toupper(letters[1:20])

clean_REDCap_dat <- as_tibble(clean_REDCap_dat)
attr(clean_REDCap_dat, "spec") <- NULL
attr(clean_REDCap_dat_dictionary, "spec") <- NULL

#### save ----

usethis::use_data(excel_ref, overwrite = TRUE)
usethis::use_data(ICD10, overwrite = TRUE)
usethis::use_data(ICD10AM, overwrite = TRUE)
usethis::use_data(telethonkids_colours, overwrite = TRUE)
usethis::use_data(telethonkids_palettes, overwrite = TRUE)
save(clean_REDCap_dat, clean_REDCap_dat_dictionary, clean_REDCap_validation, file = "tests/resources/clean_REDCap.RData")
