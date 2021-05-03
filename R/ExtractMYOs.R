#' Multi-year Observations
#'
#' This function labels multi-year observations with unique treatments, i.e. the same experiment, location, experimental vs control comparison, product and outcome.
#'
#' A identifier `UID` is given to "unique" treatment x outcome x product combinations and the number of unique growing seasons `N.Years` and
#' total number of observations `N.Obs` is calculated for each `UID` is calculated. If `N.Obs` > `N.Years` this indicates that one or more ERA
#' practices varies, but this is not captured in the ERA dataset. In these unusual circumstances we suggest averaging data for the `UID` across seasons.
#' This situation may arise where there are *levels* to an ERA practice such as different amounts of mulch application, but these were not captured in
#' the the Treatment description. The ERA team will be working to solve these issues in a future release.
#'
#`#' @param Data A prepared (ERA) dataset, see the `ERA.Prepare` function.
#' @param Prac The column name of the variable in `Data` that describes the practice. Use this parameter to choose different levels of the practice hierarchy. Default = `PrName` (Practice).
#' @param Out.Codes A vector of outcome codes to consider in the analysis. Default = `101` (Crop Yield).
#' @param MinYear An integer value for the minimum length of a MYO sequence. Sequences with fewer growing season than this number are excluded from analysis. Default = `3`.
#' @return A list of two data.tables `Risk.Means` & `Risk.Diff`, the difference between these is described under the Details section.
#' Output fields:
#' * `UID` = a unique identifier based on the field `Outcome`,`Practice`,`Practice.Base`,`Practice.Code`,`Code`,`ID`,`Site.ID`,`EU`,`T.Descrip`,`C.Descrip`,`T.NI`,`T.NO`,`C.NI`,`C.NO`,`Tree`,`Variety`,`Diversity`, and `Rep`.
#' * `N.Years`= the number of unique growing seasons reported for a value of `UID`.
#' * `N.Obs` = the total number of observation for a value of `UID`.
#' @export
ExtractMYOs<-function(Data){
  Data<-data.table(Data)
  Cols<-c("Outcome","Practice","Practice.Base","Practice.Code","plist","base.list","Code","ID","Site.ID","EU","T.Descrip","C.Descrip","T.NI","T.NO","C.NI","C.NO","Tree","Variety","Diversity","Rep")
  UID<-as.numeric(as.factor(apply(Data[,..Cols],1,FUN=function(X){paste(X,collapse="")})))
  Data[,UID:=UID]
  Data[,N.Years:=length(unique(M.Year)),by=UID]
  Data[,N.Obs:=.N,by=UID]
  return(Data)
}
