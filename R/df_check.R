
#' Dataframe check
#'
#'
#' The df_check() function is included in the package to guide the user if the input data
#' frames do not satisfy the requirements. E.g., the two data frames being analysed must have
#' an equal number of variables, i.e., columns. They must also fulfil the numeric data type
#' requirement. If any of these requirements is not fulfilled, an error message is displayed
#' indicating that a problem exists. This helps the user save time in diagnosing the problem.
#'
#' The function takes as input two data frames and displays a message of validity of data frames
#' as output.

#'
#' @param df1 First dataframe
#' @param df2 Second dataframe
#'
#' @return Returns an error message if the two dataframes do not have the same number of columns or if the elements in both dataframes are not numeric.

#' @export
#'
#' @examples
#' \dontrun{df1 <- data.frame(matrix(rnorm(100), ncol = 5))
#' df2 <- data.frame(matrix(rnorm(100), ncol = 5))
#' df_check(df1, df2)}
df_check <- function(df1, df2){

  if (sum(sapply(df1, is.numeric)) < ncol(df1)|sum(sapply(df2, is.numeric)) < ncol(df2)|(ncol(df1) != ncol(df2))){
    simpleError("Dataframes must have same ncol and must be numeric!")
  } else {
    print("Data sets are valid for analysis.")
  }
}
