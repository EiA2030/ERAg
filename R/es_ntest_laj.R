#' es_ntest_laj
#' Calculates % of effect sizes that fail Lajeunesse's (2015) modification of Geary's test (1930) test for assumption of normality. The accuracy of the sampling variance for lnRR depends on whether lnRR is normally distributed.
#' If many effect sizes fail to fulfil this relationship, then, meta- analytic results are unlikely to be robust.
#' @references Lajeunesse, M.J., 2015. Bias and correction for the log response ratio in ecological meta‐analysis. Ecology, 96(8), pp.2056-2063. doi: 10.1890/14-2402.1
#' @param CV a numeric vector of coefficient of variation for the effect sizes
#' @param Rep  numeric vector of sample sizes associated with each effect size
#' @export
es_ntest_laj<-function(CV,Rep){
  100*sum(((1/MeanC.CV)*((4*Rep^(3/2))/(1+4*Rep)))<3)/length(CV)
}
