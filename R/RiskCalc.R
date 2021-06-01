#' Calculate Risk
#'
#' This function estimates the production risk of an experimental treatment compared to the control treatment using time series data.  We suggest applying the `ERA.Prepare` function to
#' data before using with this function.
#'
#' Method:
#' We adapted a lower confidence limit (LCL) approach (e.g.  \href{https://onlinelibrary.wiley.com/doi/abs/10.2307/1243967}{Hildebrand1996},
#' \href{https://doi.org/10.1016/S0167-8809(00)00140-7}{Yamoah2000}, \href{https://doi.org/10.3763/ijas.2010.0471}{Sirrine2010}) to estimate production risk as:
#' 1) `Risk.Means` = the probability of the mean experimental treatment yielding lower than the control treatment; and
#' 2) `Risk.Diff` = the probability of the mean yield difference between experimental and control treatments being less than 0.
#' A minimum of three seasons of yield data from the same experimental treatments are required to calculate risk and this is the default threshold for the analyses presented here.
#' The minimum number of season for a multi-year observation (MYO) to be included in analyses can be adjusted using the `MinYear` parameter.
#'
#' Statistics are reported aggregated to the level of practice hierarchy specified in the `PLevel` parameter.
#'
#' Outcomes are analyzed at the subindicator level. Whilst any outcome(s) can be used with this function, it's primary purpose is to analyze productivity outcomes such as crop yield or net returns.
#'
#' Weightings are applied to mean and error estimates as per the methods described in the `ERA.Analyze` function.
#`
#' @param Data A prepared (ERA) dataset, see the `ERA.Prepare` function.
#' @param PLevel The column name of the variable in `Data` that describes the practice. Use this parameter to choose different levels of the practice hierarchy. Default = `PrName` (Practice).
#' @param Out.Codes A vector of outcome codes to consider in the analysis. Default = `101` (Crop Yield).
#' @param MinYear An integer value for the minimum length of a MYO sequence. Sequences with fewer growing season than this number are excluded from analysis. Default = `3`.
#' @return A list of two data.tables `Risk` and `Risk.Averages`.
#' `Risk` contains statistics calculated for each value of `UID` and has the fields:
#' * `UID` = a unique identifier based on the field `Outcome`,`Practice`,`Practice.Base`,`Practice.Code`,`Code`,`ID`,`Site.ID`,`EU`,`T.Descrip`,`C.Descrip`,`T.NI`,`T.NO`,`C.NI`,`C.NO`,`Tree`,`Variety`,`Diversity`, and `Rep`.
#' * `N.Years`= the number of unique growing seasons reported for each value of `UID`.
#' * `N.Obs` = the total number of observation for a value of `UID`.
#' * `Diff.Mean` = mean difference between experimental and control treatments (`mean(MeanT-MeanC)`)
#' * `Diff.SD` = standard deviation of mean difference (`sd(MeanT-MeanC)`)
#' * `Diff.t.stat` = t-statistic of  mean difference (`.Mean/(.SD/N.Obs^0.5)`)
#' * `Diff.p.val` = probability of  mean difference being <0   (`pt(Diff.t.stat,N.Obs-1,lower.tail = T)`)
#' * `Mean.C` = mean of control treatment
#' * `Mean.T` = mean of experimental treatment
#' * `Mean.T.SD` = standard deviation of experimental treatment
#' * `Mean.t.stat` = t-statistic for `MeanT` < `MeanC` (`Mean.T-Mean.C)/(Mean.T.SD/N.Obs^0.5`)
#' * `Mean.p.val` = probability of `MeanT` < `MeanC`   (`pt(Mean.t.stat,N.Obs-1,lower.tail = T)`)
#' * `N.Obs.Study` = number of observations a study (`Code` column) contributes to a combination of practice x outcome
#'
#' `Risk.Averages` is the data in the `Risk` table averaged with weighting across practice x outcome combinations. Additional fields are:
#' * `Mean.Seq.Len`= the mean value of `Risk$N.Years`
#' * `Median.Seq.Len`= the median value of `Risk$N.Years`
#' * `N.Studies` = the number of studies contributing MYOs
#' * `Total.Obs`= the total number of observations contributing to MYOs
#' * `N.Obs`= the total number of MYOs
#' * `Diff.p.val.se` = the standard error of `Risk$Diff.p.val`
#' * `Mean.p.val.se` = the standard error of `Risk$Mean.p.val`
#' @export
RiskCalc<-function(Data,
                   PLevel = "PrName",
                   Out.Codes=101,
                   MinYear=3){
  Risk<-data.table::copy(Data)

  setnames(Risk,PLevel,"Practice")
  setnames(Risk,paste0(PLevel,".Base"),"Practice.Base")
  setnames(Risk,paste0(PLevel,".Code"),"Practice.Code")
  setnames(Risk,"Out.SubInd","Outcome")

  Risk<-Risk[Outcode %in% Out.Codes]

  Risk<-ExtractMYOs(Data=Risk)
  Risk<-Risk[N.Years>=MinYear]
  # Calculate probability the mean difference (trt-control) being <0
  Risk[,Diff.Mean:=mean(MeanT-MeanC),by=UID]
  Risk[,Diff.SD:=sd(MeanT-MeanC),by=UID]
  Risk[,Diff.t.stat:=Diff.Mean/(Diff.SD/N.Obs^0.5)]
  Risk[,Diff.p.val:=pt(Diff.t.stat,N.Obs-1,lower.tail = T)]
  # Calculate probability the mean trt being < mean control
  Risk[,Mean.C:=mean(MeanC),by=UID]
  Risk[,Mean.T:=mean(MeanT),by=UID]
  Risk[,Mean.T.SD:=sd(MeanT),by=UID]
  Risk[,Mean.t.stat:=(Mean.T-Mean.C)/(Mean.T.SD/N.Obs^0.5)]
  Risk[,Mean.p.val:=pt(Mean.t.stat,N.Obs-1,lower.tail = T)]
  Risk[,N.Obs.Study:=.N,by=list(Practice,Code,Outcome)]

  Cols<-c("Outcome","Practice","Practice.Base","Practice.Code","Code","ID","Site.ID","EU","T.Descrip","C.Descrip","T.NI","T.NO","C.NI","C.NO","Tree","Variety","Diversity","Rep")
  Cols<-c(Cols,"N.Years","N.Obs","N.Obs.Study","Diff.Mean","Diff.SD","Diff.t.stat","Diff.p.val","Mean.C","Mean.T","Mean.T.SD","Mean.t.stat","Mean.p.val")
  Risk<-unique(Risk[,..Cols])
  Risk[,Weight:=((Rep^2)/(Rep*2))/N.Obs.Study]

  Risk.Means<-Risk[!is.infinite(Diff.t.stat),list(Diff.Mean=weighted.mean(Diff.Mean,Weight),
                                                  Diff.SD=weighted.mean(Diff.SD,Weight),
                                                  Diff.t.stat=weighted.mean(Diff.t.stat,Weight),
                                                  Diff.p.val=weighted.mean(Diff.p.val,Weight),
                                                  Diff.p.val.se=weighted_se(Diff.p.val,Weight),
                                                  Mean.Seq.Len=mean(N.Years),
                                                  Median.Seq.Len=median(as.numeric(N.Years)),
                                                  N.Studies=length(unique(Code)),
                                                  Total.Obs=sum(N.Obs),
                                                  N.Obs=.N),by=c("Practice","Practice.Code","Outcome")]

  Risk.Diff<-Risk[!is.infinite(Mean.t.stat),list(Mean.C=weighted.mean(Mean.C,Weight),
                                                 Mean.T=weighted.mean(Mean.T,Weight),
                                                 Mean.T.SD=weighted.mean(Mean.T.SD,Weight),
                                                 Mean.t.stat=weighted.mean(Mean.t.stat,Weight),
                                                 Mean.p.val=weighted.mean(Mean.p.val,Weight),
                                                 Mean.p.val.se=weighted_se(Mean.p.val,Weight)),
                  by=c("Practice","Practice.Code","Outcome")]


  Risk.Averages<-cbind(Risk.Means,Risk.Diff[,!c("Practice","Outcome","Practice.Code")])

  return(list(Risk=Risk,Risk.Averages=Risk.Averages))
}
