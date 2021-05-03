#' Remove Extreme Outliers
#'
#' This simple function will detect extreme outliers (values above or below 3*interquartile range) in a supplied vector of numbers.
#'
#' @param Vals A vector of numeric values.
#' @return A logical vector where `TRUE` indicate input values that are extreme outliers.
#' @export
OutCalc<-function(Vals){
  return((Vals < quantile(Vals)[2] - 3 *  IQR(Vals)  | Vals > quantile(Vals)[4] + 3 * IQR(Vals)))
}
