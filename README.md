# variant
R package used to detect changes in large datasets statistically. 



The variant package can be found at https://github.com/pgrugwiro/variant . To install and run it, use the following command: install_github(“pgrugwiro/variant”). Please note that the “devtools” package must be installed before installing a github package. 


# Test the package with the following datasets. 

df1 <- data.frame(matrix(rnorm(1000000), ncol = 100))
df2 <- data.frame(matrix(rnorm(500000), ncol = 100))
variant_signal(df1, df2, alpha = 0.5)



This package contains several helper functions: cil,cormat,df_check,fisher_trans,no_overlap,percent_overlap, and variant_signal. 
These helper functions work together to accomplish the desired computational and plotting requirements. In alphabetic order, the functions are described below. The full code for the helper functions can be found in appendix 2.
cil() 

The cil() function calculates the confidence interval parameters of a numeric vector. It takes in as input the numeric vector and the level of significance desired for the computation. As an output, it gives the population mean’s confidence interval lower and upper bounds, as well as a number that represents the length of the confidence interval, i.e., the difference between the upper bound and the lower bound of the confidence interval. 
The primary use of this function is in the graphic representation of confidence intervals. To plot these, two of these three outputs are needed. Either the lower bound and confidence length, or upper bound and confidence length, or again the lower bound and upper bound. 
cil() in use:
library(variant)

#Calculating the 95% confidence interval parameters using the cil function
vecx <- rnorm(100)
cil(vecx, 0.05)
## $lower_bound
## [1] -0.2556183
## 
## $upper_bound
## [1] 0.184271
## 
## $cil
## [1] 0.4398893

cormat()

The cormat() function is used to calculate the correlation matrix of any given numerical data frame. It is specifically designed to handle large dataset by making use of parallel computing. It takes in as input a numerical data frame of n columns and any number of rows and it returns a n x n-1 matrix of correlation coefficients. n-1 is because the correlation coefficients of any given variable with itself is removed in the output. 


cormat() in use:
library(variant)

#Calculating the coefficient of correlation matrix of a data frame.

df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
cormat(df)
##      result.1    result.2   result.3
## 1 -0.03085469 -0.03085469 0.01650608
## 2  0.01650608  0.05227340 0.05227340

df_check()

The df_check() function is included in the package to guide the user if the input data frames do not satisfy the requirements. E.g., the two data frames being analysed must have an equal number of variables, i.e., columns. They must also fulfil the numeric data type requirement. If any of these requirements is not fulfilled, an error message is displayed indicating that a problem exists. This helps the user save time in diagnosing the problem. 
The function takes as input two data frames and displays a message of validity of data frames as output. 
df_check() in use:
library(variant)

#Checking validity of data frames before analysis.

df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
df2 <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
df_check(df, df2)
## [1] "Data sets are valid for analysis."

library(variant)

#Checking validity of data frames before analysis.

df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
df2 <- data.frame(a = rnorm(100), b = rnorm(100), c = rep("a", 100))
df_check(df, df2)
## <simpleError: Dataframes must have same ncol and must be numeric!>

fisher_trans()

The fisher_trans() function is used to transform the coefficient of correlation ρ values into their z-scores using the Fisher transform. This is a necessary step before conducting parametric analyses on the correlation coefficient distribution. The function takes as input a numeric value or a vector of numeric values and returns a corresponding numeric value or vector of numeric values.
fisher_trans() in use:
library(variant)

#Transforming a coefficient of correlation into a z-score with Fisher's 
#transform.

rho = 0.89
fisher_trans(rho)
## [1] 1.421926

no_overlap()

The no_overlap() function is used to determine if two numeric vectors have overlapping confidence intervals of their population means using a desired level of significance. It takes as input two vectors and the level of significance and returns a Boolean value TRUE if no overlap exists and FALSE if overlap exists. 
no_overlap() in use:
library(variant)

#Check if two 95% confidence intervals overlap. 

vecx <- rnorm(100)
vecy <- rnorm(100)
no_overlap(vecx, vecy, 0.05)
## [1] FALSE

library(variant)

#Check if two 95% confidence intervals overlap. 

vecx <- rnorm(100)
vecy <- rnorm(100, 5, 0.5)
no_overlap(vecx, vecy, 0.05)
## [1] TRUE

percent_overlap()

The percent_overlap() function is used to calculate the overlap percentage between two numeric vectors. The function uses equation 3 described in chapter 2.  The function takes as input two numeric vectors and a desired level of significance and it outputs the percentage overlap between the confidence intervals of population means of the input vectors. If there’s no overlap of the confidence intervals, e.g., the upper bound of the confidence interval of one vector is lower than the lower bound of the confidence interval of the other vector, a negative percent overlap would be displayed. 

percent_overlap() in use:
library(variant)

#Calculate the percent overlap of the confidence intervals of population means of two      #numeric vectors 

vecx <- rnorm(100)
vecy <- rnorm(100)
percent_overlap(vecx, vecy, 0.05)
## [1] 53.9693

library(variant)

#Calculate the percent overlap of the confidence intervals of population means of two      #numeric vectors

vecx <- rnorm(100)
vecy <- rnorm(100, 5, 0.5)
percent_overlap(vecx, vecy, 0.05)
## [1] -88.86339

variant_signal()

The variant_signal() is the last and most important function in the package. It pipelines all the helper functions described above and uses their output to compute the signal. It takes as input two data frames and a desired level of significance. It then checks for the data frames validity using the df_check() function and computes the correlation coefficient matrices for both data frames, if they are valid, using the cormat() function.  The correlation coefficients are transformed into their z-scores using the fisher_trans() function. Then confidence interval parameters for the mean correlation coefficient of each variable are calculated using the cil() function, overlapping percentages and presence or absence of overlap is determined with the percent_overlap(), no_overlap() functions respectively.  The variant_signal() function then calculates average change for the non-overlapping category and compares it to the overall change. This comparison is displayed in terms of percentage and is called “signal”. Additional outputs of the variant_signal() functions are a list of percentage overlaps as computed by the percent_overlap() function, indices for which there is no overlap as determined by the no_overlap() function as well as a confidence interval plot for all the variables and for the non-overlapping variables. 

variant_signal() in use:
library(variant)

#Calculate the change in variables using variant_signal() 

df1 <- data.frame(matrix(rnorm(10000), ncol = 10))
df2 <- data.frame(matrix(rnorm(1000), ncol = 10))
variant_signal(df1, df2, 0.2)
## [1] "Data sets are valid for analysis."

## $overlap_perc
##  [1]  60.89371  71.65515  50.41643  78.57670  95.67838 -31.50166  48.33042
##  [8]  57.30070  12.15026  53.27827
## 
## $no_overlap_ind
## [1] 6
## 
## $signal
## [1] 273.6987

 







