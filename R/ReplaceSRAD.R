#' Replace missing POWER solar radiation values
#'
#' This function replaces missing solar radiation values in the POWER dataset by searching for temporally adjacent values.
#'
#' The solar radiation value of the day with the most similar rainfall to the NA value within +/- `SeqLen` days is substituted.
#'
#' @param SRad A numeric vector of solar radiation values
#' @param Rain A numeric vector of precipitation values aligned with `Rad`
#' @param SeqLen An integer value that describes how many days to look either side of an NA value for substitution.
#' @return A numeric vector of the supplied `SRad` values where NAs have been substituted with the neighboring value with the most similar rainfall amount to the
#' missing value (if non NA values are present).
#' @export
ReplaceSRAD<-function(SRad,Rain,SeqLen){

  N<-which(SRad<0|is.na(SRad))

  PSrad<-SRad
  PRain<-Rain
  a<-1


  for(i in N){

    Nseq<-c(-SeqLen:-1,1:SeqLen)

    if(i<=SeqLen){
      if(i==1){
        Nseq<-1:SeqLen
      }else{
        Nseq<-c(((1-i):-1),1:SeqLen)
      }
    }

    if(i>(length(SRad)-SeqLen)){
      if(i==length(SRad)){
        Nseq<--SeqLen:-1
      }else{
        Nseq<-c(-SeqLen:-1,1:(length(SRAD)-i))
      }
    }

    cat('\r                                                                                              ')
    cat('\r',paste("Substituting ",a,"/",length(N)))
    flush.console()

    Srad<-PSrad[i+Nseq]
    Rain<-PRain[i+Nseq]
    R<-PRain[i]

    SR<-Srad[which.min(abs(Rain - R))]

    while(is.na(SR)|SR<0){
      n<-which.min(abs(Rain - R))
      Srad<-Srad[-n]
      Rain<-Rain[-n]
      SR<-Srad[which.min(abs(Rain - R))]
    }

    if(a==1){
      ReplaceSRAD<-SR
      a<-a+1
    }else{
      ReplaceSRAD<-c(ReplaceSRAD,SR)
      a<-a+1
    }
  }

  SRad[N]<-ReplaceSRAD

  return(SRad)

}
