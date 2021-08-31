


#' Variant Signal
#' Detects the columns that changed significantly from one state in dataset 1 to another state in dataset 2.
#'
#' @param df1 dataframe 1
#' @param df2 dataframe 2
#' @param alpha level of significance e.g. 0.05
#'
#' @return list of columns that show significant change
#' @export
#'
#' @examples
#'
#' \dontrun{df1 <- data.frame(matrix(rnorm(100), ncol = 5))
#' df2 <- data.frame(matrix(rnorm(100), ncol = 5))
#' variant_signal(df1, df2, 0.05)}
#'
#'
variant_signal <- function(df1, df2, alpha){



  if (df_check(df1,df2) != "Data sets are valid for analysis.") {stop("Check datasets.")
  } else { #continue the script
    check <- "complete"
  }

  cormat_1 <- cormat(df1)
  cormat_2 <- cormat(df2)



  norm_cormat_1 <- data.frame(lapply(cormat_1, fisher_trans))
  norm_cormat_2 <- data.frame(lapply(cormat_2, fisher_trans))



  #deternmining the percent overlap

  o <- numeric()
  for (i in 1:ncol(norm_cormat_2)){
    o[i] <- percent_overlap(norm_cormat_2[,i], norm_cormat_1[,i], alpha)
  }

  which(o<0)

  o_check <- numeric()
  for (i in 1:length(norm_cormat_2)){
    o_check[i] <- no_overlap(norm_cormat_2[,i], norm_cormat_1[,i], alpha)
  }

  no <- which(o_check > 0)

  #calculating the means of each column

  col_means_1 <- sapply(norm_cormat_1, mean)
  col_means_2 <- sapply(norm_cormat_2, mean)


  #colmeans difference distribution

  diff_colmeans <- abs(col_means_1 - col_means_2)
  diff_colmeans_no <- abs(col_means_1[no] - col_means_2[no])


  #percent difference is

  perc_increase <- (mean(diff_colmeans_no) - mean(diff_colmeans))*100/mean(diff_colmeans)

  results <- list(overlap_perc = o, no_overlap_ind = no, signal = perc_increase)


  return(results)
  closeAllConnections()

}

