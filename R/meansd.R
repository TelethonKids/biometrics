#' @export

meansd <- function(x, dpp=2, dps=2){
  if(any(class(x)!="numeric" | class(x)!="interger")) {} else{print("Check you format")}
  x <- as.numeric(x)
  paste0(sprintf(paste0("%.",dpp,"f"), mean(x, na.rm=T)),
         " (",
         sprintf(paste0("%.",dps,"f"), sd(x, na.rm=T)),
         ")"
  )
}
