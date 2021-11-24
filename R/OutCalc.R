#' Remove Extreme Outliers
#'
#' This simple function will detect extreme outliers (values above or below 3*interquartile range) in a supplied vector of numbers.
#'
#' Outliers can be problematic in the analysis of ratio data when the control treatment (CT) tends to zero and the experimental treatment (ET) does not
#' Then the ET/CT ratio tends towards infinity which can unrealistically skew results’ distributions. This could occur when there are unequal impacts of
#' environmental stressors such as drought, heavy rainfall, pest or diseases between CT and ET.
#'
#' @param Vals A vector of numeric values.
#' @return A logical vector where `TRUE` indicate input values that are extreme outliers.
#' @export
OutCalc<-function(Vals){
  return((Vals < quantile(Vals)[2] - 3 *  IQR(Vals)  | Vals > quantile(Vals)[4] + 3 * IQR(Vals)))
}
