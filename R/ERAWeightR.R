#' Calculate Weightings
#'
#' This simple function will weight ERA observations based on replicates (`Reps` column) and the number of observations contributed by a study (`N.Obs.Study`)
#'  within combinations of the grouping variables specified.
#'
#' Weightings are calculated using the formula `(Rep^2/(2*Rep))/N.Obs.Study`.
#'
#' @param Data An ERA data.table containing columns `Reps` and `Code`.
#' @param Grouping.Cols A vector of columns names; these are grouping variables and weightings are calculated for each combination of their values. Default = `NA`.
#' @return ERAWeights returns the input `data.table` with two appended columns:
#' * `N.Obs.Study` = the number of observations contributed by a study (i.e., `Code`) within combinations of the grouping variables specified.
#' * `Weight.Study` = a numeric weighting.
#' @export
ERAWeight<-function(Data,Grouping.Cols){

  Data<-data.table(Data)
  if(!is.na(Grouping.Cols[1])){
    Grouping.Cols<-unique(c(Grouping.Cols,"Code"))
  }else{
    Grouping.Cols<-"Code"
  }

    Data[,N.Obs.Study:=.N,by=Grouping.Cols
        ][,Weight:=(Rep^2/(2*Rep))/N.Obs.Study]

    return(Data)
}
