#' Merge sequences
#'
#' `SOS_SeqMerge` takes a sequence created by `SOS_RSeason` where multiple non-NA values are split by sequences of NA values and in the corresponding aridity index (`AI`) data identifies the start and end point of non-NA values split by NA sequences of length `<=MaxGap`.
#'  Values from the start to end points are set to the first numeric value in the sequence. Values outwith the start and end points are set to NA.
#'
#'  For example `NA NA NA NA NA  2  2  2  2  2 NA  3  3  3 NA NA  4 NA NA` with `MaxVal=1` becomes `NA NA NA NA NA  2  2  2  2  2  2  2  2  2 NA NA NA NA NA NA` and with `MaxVal=2`
#'  becomes `NA NA NA NA NA  2  2  2  2  2  2  2  2  2  2  2  2 NA NA`.
#'
#' False starts can be dealt with using the `MinStartLen` and `MaxStartSep` arguments.
#'
#' The `ClipAI` argument determines if all sequences ends are recalculated based on on the rules provided, or just the sequences that are
#' split by NAs. If `F` then cleaned/merged sequences are likely to have a different end dekad to sequences that are not split.
#'
#' @param Seq  a vector of sequences split by NAs
#' @param AI a logical vector the same length as `Seq` indicating whether an aridity index threshold is `TRUE` or `FALSE`
#' @param MaxGap an integer value describing the maximum gap (number of NA values) allowed between non-NA values before the sequence breaks.
#' @param MinStartLen an integer value describing the minimum length of first sequence block, if the first sequence is too short and too separated from the next sequence it is removed. This is to remove false starts.
#' @param MaxStartSep an integer value describing the maximum separation of the first sequence block, if the first sequence is too short and too separated from the next sequence it is removed. This is to remove false starts.
#' @param ClipAI logical `T/F`, if `T` then `AI` values corresponding to the last non-NA value in `Seq` are all set to `F` and the sequence is halted by the last `F` value of AI.
#' @return a vector where sequences are merged across NAs and set to the first value of the series. Leading and trailing NAs remain.
#' @export
SOS_SeqMerge<-function(Seq,AI,MaxGap,MinStartLen,MaxStartSep,ClipAI){

  if(!(all(is.na(Seq))|length(unique(Seq))==1)){

    if(MaxGap>MaxStartSep){
      MaxStartSep<-MaxGap
    }

    # Set initial AI values where season start is false to false
    N2<-which(!is.na(Seq))
    if((N2[1]-1)>0){
      AI[1:(N2[1]-1)]<-F
    }

    NSeq<-length(unique(Seq[!is.na(Seq)]))
    if(!ClipAI & NSeq==1){
      N1<-which(!is.na(Seq))[1]
      N1<-c(rep(F,N1-1),rep(T,length(Seq)-N1+1))
      Seq[AI==T & N1==T]<-Seq[!is.na(Seq)][1]
    }

    Seq2<-Seq
    Seq2[is.na(Seq2)]<-99999
    X<-rle(Seq2)
    SVal<-X$values
    SLen<-X$lengths
    SN<-which(SVal!=99999)

    if(length(SN)>1){

      if(N2[length(N2)]<length(AI) & ClipAI){
        AI[(N2[length(N2)]+1):length(AI)]<-F
      }

      X<-rle(AI)
      Val<-X$values
      Length<-X$lengths
      Z<-rep(NA,length(AI))
      N<-which(Val==T)

      A<-N[1]:N[length(N)]
      # Find NA sequences in between non-NA seqs
      B<-A[!A %in% N]

      # Remove false starts
      if(Length[N[1]]<=MinStartLen & Length[B[1]]>MaxStartSep & NSeq>1){
        i<-which(AI==T)[1]

        # When is next rainfall event?
        j<-cumsum(SLen)[SN[2]-1]
        AI[i:j]<-F

        X<-rle(AI)
        Val<-X$values
        Length<-X$lengths
        Z<-rep(NA,length(AI))
        N<-which(Val==T)
        A<-N[1]:N[length(N)]
        # Find NA sequences in between non-NA seqs
        B<-A[!A %in% N]
      }

      if(length(N)>1){
        # How many consecutive B sequences <= MaxGap (starting from the first within-seq NA break)
        C<-Length[B]<=MaxGap
        if(C[1]==T){
          i<-rle(C)$lengths[1]+1
        }else{
          i<-1
        }

        j<-if(N[1]!=1){Length[1]+1}else{1} # Start of non-NA sequence
        k<-sum(Length[1:N[i]]) # End of non-NA sequence

        Z[j:k]<-Seq[!is.na(Seq)][1]

      }else{
        Z[AI==T]<-Seq[!is.na(Seq)][1]
      }

      return(Z)

    }else{

      return(Seq)
    }
  }else{
    return(Seq)
  }
}
