#' @title ICD-10-AM data dictionary
#'
#' @description
#' \code{icd10_am_dict} adds ICD-10-AM codes to icd10_dict() per guidance provided by Australian Consortium for Classification Development (\url{https://www.accd.net.au/Downloads.aspx} downloaded 8th June 2018). Mappings to ICD-10 codes are listed under 'mapping'. Note, the ICD-10-AM id values DO NOT have decimal points ".".
#'
#' @examples
#' ICD10 <- icd10_am_dict()
#'
#' ICD10["A00"]
#'
#' ICD10["A032", name]

library(biometrics)
library(data.table)

data(ICD10)
fread("data-raw/Mapping Tables - ICD-10-AM to ICD-10 - 6th Edition.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE) -> ICD10AM

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

devtools::use_data(ICD10AM, overwrite = TRUE)
