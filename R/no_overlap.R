
#' Check for CI overlap
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
