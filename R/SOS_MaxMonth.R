#' Find and Code Wettest Months
#'
#' `MaxMonth` takes 12 months of rainfall data and searches for two separate seasons with the highest rainfall.
#'
#' Rainfall data need to be a three month rolling average (n+2).
#' The `Pad` parameter sets the minimum separation distance between seasons in months (the maximum is 3).
#' The three months corresponding to the highest rainfall are coded as `1`, the next highest rainfall period
#' which does not overlap season 1 and is separated by `Pad` months is coded as `2`.
#' If two identical rainfall values exist in the 12 month series the earliest month will be chosen first.
#'
#' @param Rain a numeric vector (length = 12) of three month rolling average rainfall for a year (i.e 12 months).
#' @param Month  an integer vector (length = 12) of month numbers (1:12), if months are not in sequence the function should be able to handle this. If not supplied it is assumed months are in the order 1:12.
#' @param Pad an integer value (length = 1, values = 0:3) that sets the minimum separation distance between seasons in months (the maximum is 3).
#' @return `MaxMonth` returns a numeric vector (length = 12) with the highest rainfall season coded as `1` and the next highest encoded as `2`.
#' @export
SOS_MaxMonth<-function(Rain,Month,Pad){

  X<-rep(NA,12)

  N<-Month[Rain==max(Rain)]

  if(length(N)>1){N<-min(N)}

  N<-(N-Pad-2):(N+2+Pad)
  N[N>12]<-N[N>12]-12
  N[N<1]<-N[N<1]+12

  X[match(N,Month)]<-c(rep(99,Pad+2),rep(1,3),rep(99,Pad))

  Rain2<-Rain[is.na(X)]

  N2<-Month[is.na(X)][Rain2==max(Rain2)]
  if(length(N2)>1){N2<-min(N2)}
  N2<-N2:(N2+2)
  N2[N2>12]<-N2[N2>12]-12
  N2[N2<1]<-N2[N2<1]+12

  X[match(N2,Month)]<-rep(2,3)
  X[X==99]<-NA

  return(X)

}
