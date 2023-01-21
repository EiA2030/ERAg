#' @title lnrr_laj
#' @description Calculates log response ratio based on Taylor expansion from Jajeunesse 2011.
#' @references Nakagawa, S., et al. (2022). "A robust and readily implementable method for the meta-analysis of response ratios with and without missing standard deviations." Ecol Lett. doi: 10.1111/ele.14144
#' @param m1 Mean of treatment group 1
#' @param m2 Mean of treatment group 2
#' @param cv1_2 Coefficient of variation squared (CV^2) for treatment group 1
#' @param cv2_2 Coefficient of variation squared (CV^2) for treatment group 2
#' @param n1 Sample size for treatment group 1
#' @param n2 Sample size for treatment group 2
#'
lnrr_laj <- function(m1, m2, cv1_2, cv2_2, n1, n2){
  log(m1 / m2) + 0.5*((cv1_2 / n1) - (cv2_2 / n2))
}
