#' Find rainy seasons
#'
#' `SOS_RSeason` generates dekadal sequences where an initial rainfall condition (onset or SOS) has been met ending when an aridity condition is not met (end of season, EOS). It
#' takes two logical sequences of dekadal information `RAIN` and `AI` and determines rainy seasons from these.
#'
#' The `RAIN` parameter indicates where a starting condition of rainfall is `TRUE`. The `AI` parameter indicates an ending condition
#' where the aridity index is `FALSE`. The growing season occurs for sequences starting when `AI` and `RAIN` are
#' `TRUE` and ending when `AI` is `FALSE.`
#'
#' The typical WRSI onset date definition is a dekad with 25 mm followed by two dekads with a total of 20mm.
#' The typical WRSI end of season definition is when the long-term mean aridity index drops below 0.5 (i.e. mean potential evapotranspiration >= 2*mean rainfall).
#'
#' @param RAIN  a logical vector indicating where a starting condition of rainfall is `TRUE`
#' @param AI  a logical vector indicating whether an aridity index threshold is `TRUE` or `FALSE`
#' @return integer sequence
#' @export
SOS_RSeason<-function(RAIN,AI){

  # Remove instances at start of AI==T sequence where RAIN==F
  SeqFun2<-function(AI,RAIN,SEQ){
    AI[is.na(AI)]<-F
    RAIN[is.na(RAIN)]<-F
    N<-which(AI==T & RAIN==T)[1] # Find first instance where both are true, if index is >1 then this means that there is an instance of F/T
    SEQ<-rep(SEQ,length(AI))

    # If all instance of AI & RAIN are F then the rain threshold was not met and there is no sequence.
    if(is.na(N)){
      SEQ<-NA
    }else{
      if(N>1){
        SEQ[1:(N-1)]<-NA  # Set T/F values to NA
      }
    }
    return(SEQ)
  }

  # Find T sequences
  SeqLen<-rle(AI)
  Lengths<-SeqLen$lengths
  Values<-SeqLen$values


  # If all values = F then there was no sequence
  if(all(Values==F)){
    Seq<-as.integer(rep(NA,length(RAIN)))
  }else{
    Y<-rep(NA,length(AI))
    Y[AI==T & !is.na(AI)]<-rep(1:sum(Values,na.rm = T),Lengths[Values==T & !is.na(Values)])

    Seq<-data.table(AI=AI,RAIN=RAIN,SEQ=Y)

    Seq<-Seq[!is.na(SEQ),SEQ:=SeqFun2(AI,RAIN,SEQ),by=SEQ][,SEQ]
  }

  return(Seq)
}
