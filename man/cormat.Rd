% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cormat.R
\name{cormat}
\alias{cormat}
\title{Correlation matrix}
\usage{
cormat(df)
}
\arguments{
\item{df}{dataframe for which a correlation matrix is to be calculated}
}
\value{
Correlation matrix
}
\description{
The cormat() function is used to calculate the correlation matrix of any given numerical
data frame. It is specifically designed to handle large dataset by making use of parallel
computing. It takes in as input a numerical data frame of n columns and any number of rows
and it returns a n x n-1 matrix of correlation coefficients. n-1 is because the correlation
coefficients of any given variable with itself is removed in the output.
}
\examples{
\dontrun{cormat(data.frame(matrix(rnorm(100), ncol = 5)))
closeAllConnections()}
}
