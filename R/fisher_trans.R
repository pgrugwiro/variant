#' Fisher's Transformation.
#'
#'
#' The fisher_trans() function is used to transform the coefficient of correlation ρ values into
#' their z-scores using the Fisher transform. This is a necessary step before conducting parametric
#' analyses on the correlation coefficient distribution. The function takes as input a numeric
#' value or a vector of numeric values and returns a corresponding numeric value or vector of
#' numeric values.
#'
#'
#' @param r is the correlation coefficient to be transformed into a z score
#'
#' @return z-score
#' @export
#'
#' @examples
#' \dontrun{fisher_trans(0.89)}
fisher_trans <- function(r){
  z <- 0.5*(log(1+r) - log(1-r))
  return(z)
}
