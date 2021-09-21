#' Check for CI overlap
#'
#'
#' The no_overlap() function is used to determine if two numeric vectors have overlapping
#' confidence intervals of their population means using a desired level of significance.
#' It takes as input two vectors and the level of significance and returns a Boolean value
#' TRUE if no overlap exists and FALSE if overlap exists.
#'
#'
#' @param x vector x
#' @param y vector y
#' @param alpha level of significance alpha
#'
#' @return indices where no confidence interval overlap exists
#' @export
#'
#' @examples
#' vecx <- rnorm(100)
#' vecy <- rnorm(100)
#' no_overlap(vecx, vecy, 0.05)
#'
no_overlap <- function(x,y, alpha){
  z <- qnorm(1-alpha/2)
  check <- abs(mean(x) - mean(y)) > z*(sd(x)/sqrt(length(x)) + sd(y)/sqrt(length(y)))
  return(check)
}
