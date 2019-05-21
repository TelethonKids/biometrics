#' @title ICD-10 data dictionary
#'
#' @description
#' \code{icd10_dict} imports ICD-10 XML data provided by GNU Health (\url{http://health.gnu.org/} downloaded June 2018) and creates a data table with ID as key. Note, the ICD-10 id values DO NOT have decimal points ".".
#'
#' @examples
#' ICD10 <- icd10_dict()
#'
#' ICD10["A00"]
#'
#' ICD10["A032", name]

library(XML)
library(data.table)

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

devtools::use_data(ICD10, overwrite = TRUE)
