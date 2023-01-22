#' v_lnrr_naka
#' Calculates the sampling variance for log response ratio based on second order Taylor expansion proposed by Lajeunesse 2011 as per equation 7 of Nagakawa 2022
#' @references Nakagawa, S., et al. (2022). "A robust and readily implementable method for the meta-analysis of response ratios with and without missing standard deviations." Ecol Lett. doi: 10.1111/ele.14144
#' @param CV1_data Vector of coefficient of variations for treatment group 1 (for those observations with CV)
#' @param CV2_data Coefficient of variation for treatment group 2 (for those observations with CV)
#' @param n1 Sample size for treatment group 1
#' @param n2 Sample size for treatment group 2
#' @param n1_data Vector of sample sizes for treatment group 1 (for those observations with CV)
#' @param n2_data Vector of sample sizes for treatment group 2 (for those observations with CV)
#' @export
v_lnrr_naka<-function(n1,n2,n1_data,n2_data,CV1_data,CV2_data){
  (((sum(CV1_data*n1_data)/sum(n1_data))^2)/n1)+
    (((sum(CV2_data*n2_data)/sum(n2_data))^2)/n2)+
    (((sum(CV1_data*n1_data)/sum(n1_data))^4)/(2*n1^2))+
    (((sum(CV2_data*n2_data)/sum(n2_data))^4)/(2*n2^2))
}
