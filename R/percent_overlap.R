#' Confidence Interval Overlap
#'
#' @param x a numeric vector
#' @param y a numeric vector
#' @param alpha level of significance
#'
#' @return percentage overlap between the confidence intervals of the two numeric vectors
#' @export
#'
#' @examples
#'
#' \dontrun{vecx <- rnorm(100)
#' vecy <- rnorm(100)
#' percent_overlap(vecx, vecy, 0.05)}
#'
#'
percent_overlap <- function(x, y, alpha){
  z <- qnorm(1-alpha/2)
  K <- 1+ (sd(x)/sqrt(length(x)))/(sd(y)/sqrt(length(y)))
  if (mean(x) - mean(y) >= 0){
    omega <- (z*sd(y)*K/sqrt(length(y)) - (mean(x)-mean(y)))*100/(z*sd(y)*K/sqrt(length(y)) + (mean(x)-mean(y)))
  } else {
    omega <- (z*sd(y)*K/sqrt(length(y)) - (mean(y)-mean(x)))*100/(z*sd(y)*K/sqrt(length(y)) + (mean(y)-mean(x)))
  }
  return(omega)
}
