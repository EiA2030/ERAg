#' @title v_lnrr_laj
#' @description Calculates the sampling variance for log response ratio based on second order Taylor expansion proposed by Lajeunesse 2011
#' @references Nakagawa, S., et al. (2022). "A robust and readily implementable method for the meta-analysis of response ratios with and without missing standard deviations." Ecol Lett. doi: 10.1111/ele.14144
#' @param cv1_2 Coefficient of variation squared (CV^2) for treatment group 1
#' @param cv2_2 Coefficient of variation squared (CV^2) for treatment group 2
#' @param n1 Sample size for treatment group 1
#' @param n2 Sample size for treatment group 2
v_lnrr_laj <- function(cv1_2, cv2_2, n1, n2){
  ((cv1_2) / n1) + ((cv2_2) / n2) +
    ((cv1_2)^2 / (2*n1)^2) + ((cv2_2)^2 / (2*n2)^2)
}
