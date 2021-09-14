


#' Confidence interval length
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
