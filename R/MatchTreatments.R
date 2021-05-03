#' Match Observations
#'
#' This simple function will add a identfication field `MID` to the table supplied for unique combinations of the grouping columns supplied.
#'
#' For example, if you wish to know which outcomes are shared between the same pairings of experimental and control treatments you could specify
#' `Match.Cols = c("Code","Site.ID","C.Descrip","T.Descrip","SubPrName","SubPrName.Base","M.Year")` and `Focal.Col = Out.SubInd` this would give a unique
#' `MID` to observations that have the same control and treatment practices, location, growing season and study. The `N.Focal` column of the output
#'  dataset would tell you how many unique values of `Out.SubInd` are present in each group, and `Focal.List` would concatenate
#'  these values.
#`
#' @param Data A prepared (ERA) dataset, see the `ERA.Prepare` function.
#' @param Match.Cols The column name of the variable in `Data` that describes the practice. Use this parameter to choose different levels of the practice hierarchy. Default = `PrName` (Practice).
#' @param Focal.Col A vector of outcome codes to consider in the analysis. Default = `101` (Crop Yield).
#' @return A data.table with three columns appended::
#' * `MID` = a unique identifier for each combination of the grouping variables (column names) supplied `Match.Cols`
#' * `N.Focal`= the number of unique values of `Focal.Col` for each value of `MID`
#' * `Focal.List` = unique values of `Focal.Col` concatenated with a `-` delim calculate for each value `MID`
#' @export
MatchTreatments<-function(Data,Match.Cols,Focal.Col){
  Data[,MID:=as.numeric(as.factor(apply(Data[,..Match.Cols],1,FUN=function(X){paste(X,collapse="")})))]
  X<-Data[,..Focal.Col]
  setnames(Data,Focal.Col,"Focal.Col")
  Data[,N.Focal:=length(unique(Focal.Col)),by=MID]
  Data[,Focal.List:=paste(sort(unique(Focal.Col)),collapse="-"),by=MID]
  Data[,XXXX:=X]
  setnames(Data,"XXXX",Focal.Col)
  Data[,Focal.Col:=NULL]

  return(Data)
}
