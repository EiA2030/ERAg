#' lnrr_naka
#' Calculates log response ratio based on Taylor expansion from Lajeunesse 2011 as per equation 6 of Nagakawa 2022.
#' @references Nakagawa, S., et al. (2022). "A robust and readily implementable method for the meta-analysis of response ratios with and without missing standard deviations." Ecol Lett. doi: 10.1111/ele.14144
#' @param m1 Mean of treatment group 1
#' @param m2 Mean of treatment group 2
#' @param CV1_data Vector of coefficient of variations for treatment group 1 (for those observations with CV)
#' @param CV2_data Coefficient of variation for treatment group 2 (for those observations with CV)
#' @param n1 Sample size for treatment group 1
#' @param n2 Sample size for treatment group 2
#' @param n1_data Vector of sample sizes for treatment group 1 (for those observations with CV)
#' @param n2_data Vector of sample sizes for treatment group 2 (for those observations with CV)
#' @export
lnrr_naka<-function(m1,m2,n1,n2,n1_data,n2_data,CV1_data,CV2_data){
  log(m1/m2)+0.5*(
    ((sum(n1_data*CV1_data)/sum(n1_data)^2)/n1)-
      ((sum(n2_data*CV2_data)/sum(n2_data)^2)/n2)
  )
}
