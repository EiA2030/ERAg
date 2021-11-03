#' Prepare Data for Chord Plot
#'
#' This function prepares ERA data for use with the `ERAChordPlot` function. Chord values based on response ratios and vote counting are calculated for each
#' combination of values in the `from` x `to` fields specified aggregated spatially by the field specified in the `ALevel` function..
#'
#' Column names in the ERA dataset (e.g. `ERA.Compiled`) must be standardized using the `StandColNames` function before use with this function.
#'
#' @param Data  a `data.frame` or `data.table` of ERA data with column names standardized using the `StandColNames` function.
#' @param OutcomeCodes A vector of outcome codes to consider in the analysis. Default = `101` (Crop Yield).
#' @param from A character vector of length one defining the `from` column in the returned data to be supplied to the \link[circlize]{chordDiagram} function.
#' @param to A character vector of length one defining the `to` column in the returned data to be supplied to the \link[circlize]{chordDiagram} function.
#' @param Min.Chord.Perc A numeric vector of length one defining the minimum chord width to be considered, values less than `Perc.Neg` are combined and renamed to `Other`.
#' @param ALevel  A character vector of length one containing a value from `ERAg::ERAConcepts$Agg.Levels$Agg`; this indicates the of spatial aggregation over which data are summarized.
#' @return a `data.table` with row indicating from-to object pairs and columns:
#' *`from` = objects specified in `from` argument
#' *`to` = objects specified in `to` argument
#' *`value` = width of chord (e.g. number of unique instances of `ALevel` for from-to object pair)
#' *`yi` = strength of relationship for from-to object pair as response ratio
#' *`value` = strength of relationship for from-to object pair as response as vote count
#' @export
#' @import data.table
#' @importFrom data.table copy
PrepareChordData<-function(Data,
                           OutcomeCodes,
                           from,
                           to,
                           Min.Chord.Perc=1,
                           ALevel){

  OutcomeCodes<-data.table(ERAg::OutcomeCodes)
  Vars<-c(from,to)
  Chord.Data<-data.table::copy(Data)


  # Add weightings
  Chord.Data[,N.Obs:=.N,by=c(from,to,ALevel)][,Weighting:=(Rep^2/(2*Rep))/N.Obs]

  Chord.Data<-Chord.Data[,yi:=log(MeanT/MeanC)][!(Practice==""|is.na(Practice))]

  Chord.Data[is.infinite(yi),yi:=NA]
  Chord.Data[is.nan(yi),yi:=NA]

  setnames(Chord.Data,Vars[1],"from")
  setnames(Chord.Data,Vars[2],"to")

  Chord.Data<-Chord.Data[,list(value=.N,
                               value.from=sum(Chord.Data[,from]==from),
                               value.to=sum(Chord.Data[,to]==to),
                               yi=weighted.mean(yi,w=Weighting,na.rm=T),
                               vote=(sum(MeanT>MeanC,na.rm=T)-sum(MeanT<MeanC,na.rm=T))/.N),by=list(from,to)]

  Chord.Data[,pc.from:=round(100*value.from/sum(value),2)]
  Chord.Data[pc.from<Min.Chord.Perc,from:="Other"]


  Chord.Data[,pc.to:=round(100*value.to/sum(value),2)]
  Chord.Data[pc.to<Min.Chord.Perc,to:="Other"]

  Chord.Data[,value.from:=NULL][,value.to:=NULL][,pc.to:=NULL][,pc.from:=NULL]


  Chord.Data<-unique(Chord.Data[,list(value=sum(value),yi=stats::weighted.mean(yi,w=value,na.rm=T),vote=stats::weighted.mean(vote,w=value,na.rm=T)),by=list(from,to)])

  Chord.Data[,from:=gsub(" ","\n",from)]
  Chord.Data[,from:=gsub(".Code","",from)]
  Chord.Data[,to:=gsub(" ","\n",to)]

  return(Chord.Data)
}
