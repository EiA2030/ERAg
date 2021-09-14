#' Classify outcomes with negative numbers that are incompatible with ratio approaches
#'
#' For a the supplied outcome argument `OCode` this function classifies the difference between `MeanC` and `MeanT` into character and numeric columns based on the
#' `Threshold` and `Vals` arguments. These columns are appended to the input dataset that has been subset to the outcome of interest.
#'
#' When generating ratios the function will invert outcomes where `MeanT` and `MeanC` are both negative unless the `Invert2xNeg` argument is set to `FALSE`.
#'
#' Where MeanC is NA and MeanT is populated this function will base classfications on the MeanT value only.
#'
#' @param Data An ERA data.table (e.g. `ERAg::ERA.Compiled`).
#' @param OCode An ERA outcome code (see `ERAg::OutcomeCodes[,Codes]`.
#' @param Thresholds A numeric vector of length four corresponding to classification thresholds for worst, poor, good & best outcomes. If:
#' * `MeanT/MeanC<Threshold[1]` then Class = `--`
#' * `Threshold[1]<=MeanT/MeanC<Threshold[2]` then Class = `-`
#' * `Threshold[2]<=MeanT/MeanC<Threshold[3]` then Class = `0`
#' * `Threshold[3]<=MeanT/MeanC<Threshold[4]` then Class = `+`
#' * `MeanT/MeanC>=Threshold[4]`` then Class = `++`
#' @param Vals A numeric vector of length 11 proving numeric scores for each outcome classification:
#' * `Vals[1]`: Class `0` = `MeanT/MeanC<Threshold[1]`
#' * `Vals[2]`: Class `++` = `Threshold[1]<=MeanT/MeanC<Threshold[2]`
#' * `Vals[3]`: Class `+`= `Threshold[2]<=MeanT/MeanC<Threshold[3]`
#' * `Vals[4]`: Class `-` = `Threshold[3]<=MeanT/MeanC<Threshold[4]`
#' * `Vals[5]`: Class `--` = `MeanT/MeanC>=Threshold[4]`
#' * `Vals[6]`: Class `+-` = `MeanT>0 & MeanC<0`
#' * `Vals[7]`: Class `-+` =` MeanT<0 & MeanC>0`
#' * `Vals[8]`: Class `+0` = `MeanT>0 & MeanC==0`
#' * `Vals[9]`: Class `0+` = `MeanT==0 & MeanC>0`
#' * `Vals[10]`: Class `-0` = `MeanT<0 & MeanC==0`
#' * `Vals[11]`: Class `0-` = `MeanT==0 & MeanC<0`
#' @param Invert2xNeg Logical `T/F`. Where MeanC<0 & MeanT<0 invert the calculation of yi to `log(MeanC/MeanT)`
#' @return ClassifyNegVals returns the input Data subset to the outcome provided in the `OCode`argument and with the additional columns:
#' * `Class` a character vector of class names
#' * `Class.Val` a numeric vector of class values
#' @export
ClassifyNegVals<-function(Data,
                       OCode,
                       Thresholds=c(0.7,0.95,1.05,1.3),
                       Vals=c("0"=0,"++"=2,"+"=1,"-"=-1,"--"=-2,"+-"=1,"-+"=-1,"+0"=1,"0+"=-1,"-0"=-1,"0-"=1),
                       Invert2xNeg=T
                       ){

  Y<-data.table::copy(Data)
  X<-Y[Outcode %in% OCode]

  # Calculate response ratio (yi)
  X[,yi:=log(MeanT/MeanC)][,list(MeanC,MeanT)]

  if(Invert2xNeg){
  # Invert yi for double negative outcomes
  X[MeanC<0 & MeanT<0,yi:=log(MeanC/MeanT)]
  }

  # Set percentage change to NA
  X[,pc:=NA]

  # Add character classification
  X[,Class:="0"
  ][yi>=log(Thresholds[3]),Class:="+"
  ][yi>=log(Thresholds[4]),Class:="++"
  ][yi<=log(Thresholds[2]),Class:="-"
  ][yi<=log(Thresholds[1]),Class:="--"
  ][MeanC<0 & MeanT>0,Class:="+-"
  ][MeanC>0 & MeanT<0,Class:="-+"
  ][MeanC==0 & MeanT>0,Class:="+0"
  ][MeanC>0 & MeanT==0,Class:="0+"
  ][MeanC==0 & MeanT<0,Class:="-0"
  ][MeanC<0 & MeanT==0,Class:="0-"]

  # If only MeanT value present
  X[is.na(MeanC) & !is.na(MeanT),Class:="0"
  ][is.na(MeanC) & MeanT>=log(Thresholds[3]),Class:="+"
  ][is.na(MeanC) & MeanT>=log(Thresholds[4]),Class:="++"
  ][is.na(MeanC) & MeanT<=log(Thresholds[2]),Class:="-"
  ][is.na(MeanC) & MeanT<=log(Thresholds[1]),Class:="--"]


  # Add ordinal numeric classification
  X[,Class.Val:=Vals[1]
  ][yi>=log(Thresholds[3]),Class.Val:=Vals[2]
  ][yi>=log(Thresholds[4]),Class.Val:=Vals[3]
  ][yi<=log(Thresholds[2]),Class.Val:=Vals[4]
  ][yi<=log(Thresholds[1]),Class.Val:=Vals[5]
  ][MeanC<0 & MeanT>0,Class.Val:=Vals[6]
  ][MeanC>0 & MeanT<0,Class.Val:=Vals[7]
  ][MeanC==0 & MeanT>0,Class.Val:=Vals[8]
  ][MeanC>0 & MeanT==0,Class.Val:=Vals[9]
  ][MeanC==0 & MeanT<0,Class.Val:=Vals[10]
  ][MeanC<0 & MeanT==0,Class.Val:=Vals[11]]

  # If only MeanT value present
  X[is.na(MeanC) & !is.na(MeanT),Class.Val:=Vals[1]
  ][is.na(MeanC) & MeanT>=log(Thresholds[3]),Class.Val:=Vals[2]
  ][is.na(MeanC) & MeanT>=log(Thresholds[4]),Class.Val:=Vals[3]
  ][is.na(MeanC) & MeanT<=log(Thresholds[2]),Class.Val:=Vals[4]
  ][is.na(MeanC) & MeanT<=log(Thresholds[1]),Class.Val:=Vals[5]]

  # Remove duplicate columns
  X[,MeanC:=NULL][,MeanT:=NULL]

  # Set infinite values to NA
  X[is.infinite(yi),yi:=NA]

  # Merge Classes with supplied dataset
  Y<-cbind(Y[,yi:=X[,yi]],X[,!"yi"])

  return(Y)

}
