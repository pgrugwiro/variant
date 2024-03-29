% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fisher_trans.R
\name{fisher_trans}
\alias{fisher_trans}
\title{Fisher's Transformation.}
\usage{
fisher_trans(r)
}
\arguments{
\item{r}{is the correlation coefficient to be transformed into a z score}
}
\value{
z-score
}
\description{
The fisher_trans() function is used to transform the coefficient of correlation ρ values into
their z-scores using the Fisher transform. This is a necessary step before conducting parametric
analyses on the correlation coefficient distribution. The function takes as input a numeric
value or a vector of numeric values and returns a corresponding numeric value or vector of
numeric values.
}
\examples{
\dontrun{fisher_trans(0.89)}
}
