#' Give season sequences in a time-series unique values
#'
#' `SOS_UniqueSeq` takes season sequences (i.e., values of 1 and 2 separated by NAs) and gives each season a unique integer value.
#'
#' @param Data an integer vector of seasons (sequences of `1` and `2` separated by NAs)
#' @return integer unique sequence
#' @export
SOS_UniqueSeq<-function(Data){
  Y<-rle(Data)
  Seq<-rep(NA,length(Data))
  if(!all(is.na(Y$values))){
    Seq[!is.na(Data)]<-rep(1:sum(!is.na(Y$values)),Y$lengths[!is.na(Y$values)])
  }
  return(Seq)
}
