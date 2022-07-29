#' glm_rr
#'
#' Returns summary table output for a glm model, taking care of exponentiating the coefficients and confidence intervals
#'
#' @param mod output from a glm model (with family binomial or Poisson)
#' @param digits (default 2) the number of decimal points to be used across the output
#'
#' @return Model output, in summary form
#'
#' @importFrom stats sd
#'
#' @examples
#' \dontrun{
## Dobson (1990) Page 93: Randomized Controlled Trial :
#'  counts <- c(18,17,15,20,10,20,25,13,12)
#'  outcome <- gl(3,1,9)
#'  treatment <- gl(3,3)
#'  data.frame(treatment, outcome, counts) # showing data
#'  glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
#'
#'  summary(glm.D93)
#'  glm.rr(glm.D93)
#' }
#'
#' @export
glm_rr <- function(mod, digits = 2) {

  if (mod$family$family == "binomial") {
    LABEL <- "OR"
  } else if (mod$family$family == "poisson") {
    LABEL <- "RR"
  } else {
    stop("Not logistic or Poisson model")
  }

  COEF      <- stats::coef(mod)
  CONFINT   <- stats::confint(mod)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)

  colnames(TABLE.EXP)[1] <- LABEL

  TABLE.EXP
}
