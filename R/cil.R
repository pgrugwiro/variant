#' Confidence interval length
#'
#'
#' The cil() function calculates the confidence interval parameters of a numeric vector.
#' It takes in as input the numeric vector and the level of significance desired for the
#' computation. As an output, it gives the population mean’s confidence interval lower and
#' upper bounds, as well as a number that represents the length of the confidence interval,
#' i.e., the difference between the upper bound and the lower bound of the confidence interval.
#'
#'
#' @param x numeric vector
#' @param alpha level of significance
#'
#' @return confidence interval length at the given level of significance
#' @export
#'
#' @examples
#'
#' \dontrun{vecx <- rnorm(100)
#' cil(vecx, 0.05)}
#'
cil <- function(x, alpha){
  z <- qnorm(1-alpha/2)
  ci <- 2*z*sd(x)/sqrt(length(x))
  lb <- mean(x) - ci/2
  ub <- mean(x) + ci/2
  results <- list(lower_bound = lb, upper_bound = ub, cil = ci)

  return(results)
}
