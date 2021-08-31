

#' Fisher's Transformation.
#' Transforms the correlation "r" values into z-scores using the Fisher's transformation formula.
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
