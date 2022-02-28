#' Pad season sequences
#'
#' `SOS_SeasonPad` takes season sequences (i.e., values of 1 and 2 separated by NAs) and increases the sequence length in forward and backwards directions.
#'
#' @param Data an integer vector of seasons (sequences of `1` and `2` separated by NAs)
#' @param PadBack an integer value specifying number of places to increase sequence backwards. This should not be greater than the minimum separation of sequences.
#' @param PadForward an integer value specifying number of places to increase sequence forwards. This should not be greater than the minimum separation of sequences.
#' @return integer season
#' @export
SOS_SeasonPad<-function(Data,PadBack,PadForward){
  Data[is.na(Data)]<-99

  Seq1<-c(rep(99,PadBack),1)
  Seq2<-c(rep(99,PadBack),2)
  Seq3<-c(1,rep(99,PadForward))
  Seq4<-c(2,rep(99,PadForward))

  N <- which(Data == Seq1[1])
  N<-N[sapply(N, function(i) all(Data[i:(i+(length(Seq1)-1))] == Seq1))]
  N<-N[!is.na(N)]
  N<-c(N,N+1,N+2,N+3)
  N[N>length(Data)]<-NULL
  Data[N]<-1

  N <- which(Data == Seq3[1])
  N<-N[sapply(N, function(i) all(Data[i:(i+(length(Seq3)-1))] == Seq3))]
  N<-N[!is.na(N)]
  N<-c(N,N+1,N+2,N+3)
  N[N>length(Data)]<-NULL
  Data[N]<-1


  if(2 %in% Data){
    N <- which(Data == Seq2[1])
    N<-N[sapply(N, function(i) all(Data[i:(i+(length(Seq2)-1))] == Seq2))]
    N<-N[!is.na(N)]
    N<-c(N,N+1,N+2,N+3)
    N[N>length(Data)]<-NULL
    Data[N]<-2

    N <- which(Data == Seq4[1])
    N<-N[sapply(N, function(i) all(Data[i:(i+(length(Seq4)-1))] == Seq4))]
    N<-N[!is.na(N)]
    N<-c(N,N+1,N+2,N+3)
    N[N>length(Data)]<-NULL
    Data[N]<-2
  }

  Data[Data==99]<-NA

  return(Data)
}
