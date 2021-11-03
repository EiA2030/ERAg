#' Match Observations
#'
#' This simple function will add a identification field `MID` to the table supplied for unique combinations of the grouping columns supplied.
#'
#' For example, if you wish to know which outcomes are shared between the same pairings of experimental and control treatments you could specify
#' `Match.Cols = c("Code","Site.ID","C.Descrip","T.Descrip","SubPrName","SubPrName.Base","M.Year")` and `Focal.Col = Out.SubInd` this would give a unique
#' `MID` to observations that have the same control and treatment practices, location, growing season and study. The `N.Focal` column of the output
#'  dataset would tell you how many unique values of `Out.SubInd` are present in each group, and `Focal.List` would concatenate
#'  these values.
#'
#' @param Data An ERA dataset (e.g. `ERAg::ERACompiled`)
#' @param Match.Cols A character vector of column names that must all contain identical values for rows in the dataset to match (i.e. share the same `MID`)
#' @param Focal.Col A character vector of a single column name; the number of unique values of `Focal.Col` for each combination of value of `Match.Cols` values
#' (`MID`) are returned along with a concatenated string of the values.
#' @return A data.table with three columns appended::
#' * `MID` = a unique identifier for each combination of the grouping variables (column names) supplied `Match.Cols`
#' * `N.Focal`= the number of unique values of `Focal.Col` for each value of `MID`
#' * `Focal.List` = unique values of `Focal.Col` concatenated with a `-` delim calculate for each value `MID`
#' @export
#' @import data.table
MatchTreatments<-function(Data,
                          Match.Cols,
                          Focal.Col){
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
