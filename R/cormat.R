

#' Correlation matrix:
#' Calculates and returns the correlation matrix of a dataframe. Specifically designed to work with large dataset using parallelization tools.
#' @param df dataframe for which a correlation matrix is to be calculated
#'
#' @return Correlation matrix
#' @export
#'
#' @examples
#' \dontrun{cormat(data.frame(matrix(rnorm(100), ncol = 5)))
#' closeAllConnections()}

cormat <- function(df){
  #If system limits the number of cores to be used to 2, set number of cores
  #to 2, otherwise, use all available cores:

  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores
    numCores <- 2L
  } else {
    # use all cores
    numCores <- parallel::detectCores()
  }

  #Register the number of cores with doParallel
  doParallel::registerDoParallel(numCores)


  #Define the foreach operands
  `%dopar%` <- foreach::`%dopar%`
  `%do%` <- foreach::`%do%`

  #Function to calculate the correlation matrix
  mat <- as.data.frame(
    foreach::foreach(b = df, .combine='cbind', .packages = "doParallel") %dopar% {
      foreach::foreach(a = df, .combine='c') %do% {
        cor(a, b)
      }
    }

  )

  #Removing the diagonal values of 1, which could affect/skew the data
  #Uses the function "near" in order to ensure all the 1s are removed (machine tolerance).

  mat <- data.frame(sapply(mat, function(x) x <- x[!dplyr::near(x,1)]))
  return(mat)
  closeAllConnections()
}




