


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


  #calculating confidence intervals length

  norm_cormat_1_cil <- as.vector(unlist(sapply(norm_cormat_1, cil, 0.05)[3,]))
  norm_cormat_2_cil <- as.vector(unlist(sapply(norm_cormat_2, cil, 0.05)[3,]))



   #calculating the means of each column

  col_means_1 <- sapply(norm_cormat_1, mean)
  col_means_2 <- sapply(norm_cormat_2, mean)


  #colmeans difference distribution

  diff_colmeans <- abs(col_means_1 - col_means_2)
  diff_colmeans_no <- abs(col_means_1[no] - col_means_2[no])


  if (length(no) != 0){

    #percent difference is

    perc_increase <- (mean(diff_colmeans_no) - mean(diff_colmeans))*100/mean(diff_colmeans)
    results <- list(overlap_perc = o, no_overlap_ind = no, signal = perc_increase)

    #graphic

    lower_bound <- min(c(col_means_1-norm_cormat_1_cil/2,col_means_2-norm_cormat_2_cil/2)) -
      .2*mean(c(col_means_1-norm_cormat_1_cil/2,col_means_2-norm_cormat_2_cil/2))

    upper_bound <- max(c(col_means_1+norm_cormat_1_cil/2,col_means_2+norm_cormat_2_cil/2)) +
      .2*mean(c(col_means_1-norm_cormat_1_cil/2,col_means_2-norm_cormat_2_cil/2))


    opar <- par(no.readonly = T, mar = c(2,2,2,2))
    par(mfrow = c(2,1))
    plot(1, type="n", xlab="Col Index", ylab="C.I.", main = "All Columns", cex.main = 0.9,
         xlim=c(0, length(col_means_2)), ylim=c(lower_bound, upper_bound))
    for (i in 1:length(col_means_1)) lines(c(i,i),
                                           c(col_means_1[i]-norm_cormat_1_cil[i]/2,
                                             col_means_1[i]+norm_cormat_1_cil[i]/2), col = "blue")
    for (i in 1:length(col_means_2)) lines(c(i,i),
                                           c(col_means_2[i]-norm_cormat_2_cil[i]/2,
                                             col_means_2[i]+norm_cormat_2_cil[i]/2), col = "red")



    lower_bound_no <- min(c(col_means_1[no]-norm_cormat_1_cil[no]/2,col_means_2[no]-norm_cormat_2_cil[no]/2)) -
      .2*mean(c(col_means_1[no]-norm_cormat_1_cil[no]/2,col_means_2[no]-norm_cormat_2_cil[no]/2))

    upper_bound_no <- max(c(col_means_1[no]+norm_cormat_1_cil[no]/2,col_means_2[no]+norm_cormat_2_cil[no]/2)) +
      .2*mean(c(col_means_1[no]-norm_cormat_1_cil[no]/2,col_means_2[no]-norm_cormat_2_cil[no]/2))


    plot(1, type="n", xlab="", ylab="C.I.", main = "Signal (Non-Overlapping) Columns", cex.main = 0.9,
         xlim=c(0, length(col_means_2[no])), ylim=c(lower_bound_no, upper_bound_no))
    for (i in 1:length(col_means_1[no])) lines(c(i,i),
                                               c(col_means_1[no][i]-norm_cormat_1_cil[no][i]/2,
                                                 col_means_1[no][i]+norm_cormat_1_cil[no][i]/2), col = "blue")
    for (i in 1:length(col_means_2[no])) lines(c(i,i),
                                               c(col_means_2[no][i]-norm_cormat_2_cil[no][i]/2,
                                                 col_means_2[no][i]+norm_cormat_2_cil[no][i]/2), col = "red")


    par(opar)


  } else {


    #percent difference is

    perc_increase <- "No change"
    results <- list(overlap_perc = o, signal = perc_increase)


    #graphic

    lower_bound <- min(c(col_means_1-norm_cormat_1_cil/2,col_means_2-norm_cormat_2_cil/2)) -
      .2*mean(c(col_means_1-norm_cormat_1_cil/2,col_means_2-norm_cormat_2_cil/2))

    upper_bound <- max(c(col_means_1+norm_cormat_1_cil/2,col_means_2+norm_cormat_2_cil/2)) +
      .2*mean(c(col_means_1-norm_cormat_1_cil/2,col_means_2-norm_cormat_2_cil/2))


    opar <- par(no.readonly = T, mar = c(2,2,2,2))
    par(mfrow = c(1,1))
    plot(1, type="n", xlab="Col Index", ylab="C.I.", main = "All Columns", cex.main = 0.9,
         xlim=c(0, length(col_means_2)), ylim=c(lower_bound, upper_bound))
    for (i in 1:length(col_means_1)) lines(c(i,i),
                                           c(col_means_1[i]-norm_cormat_1_cil[i]/2,
                                             col_means_1[i]+norm_cormat_1_cil[i]/2), col = "blue")
    for (i in 1:length(col_means_2)) lines(c(i,i),
                                           c(col_means_2[i]-norm_cormat_2_cil[i]/2,
                                             col_means_2[i]+norm_cormat_2_cil[i]/2), col = "red")



    par(opar)


  }


  return(results)

  closeAllConnections()

}

