#' Calculate Outcome Stability For Each Element of Practice x Outcome
#'
#' `StabCalc2`runs splits the `data.table` output by `PrepareStabData` into a list using the `Outcome` & `Practice` fields then applies `ERAg::StabCalc`
#' to each element of the list. The output tables are bound backtogether and output in a list.
#'
#' @param Data A data.table output by the `ERAg::PrepareStabData` function
#' @param Do.Weight logical, if `TRUE` coefficient estimates are weighted acccording to the supplied weightings in the `Data` object supplied(default = T)
#' @param Weight.by.Study logical, depreciated (default = `T`)
#' @param Rm.Out logical, if `TRUE` extreme outliers are removed withing each Practice x Outcome combination as per the method detailed in `ERAg::OutCalc` (default = `T`)
#' @param Transform logical, if `TRUE` back-transformed coefficient estimates and confidence intervals are appended to outputs  (default = `T`)
#' @param Control list, optional list of control values for the `rma.mv` estimation algorithms. If unspecified, default values are defined inside the function (default = `list(optimizer="optim",optmethod="Nelder-Mead",maxit=10000)`)
#' @param Responses character vector, this argument is depreciated do edit (default=`c("lnRR","lnVR","lnCVR")`)
#' @param Use.acv logical T/F. If `T` scale-adjusted coefficient of variation, acv, is substituted for the coefficient of variation (cv).
#' @param Inc.Prod logical T/F. If `F` analysis is conducted at the practice x outcome level (i.e. aggregated across crops). If `T` analysis is dis-aggregated to the Crop x Outcome x Practice level (Crops represented by the `EU` column in the `Data parameter`)
#' @param Cores integer. The number of cores to use for parallel processing, if set to `1` no parallel processing is applied.
#' @return `StabCalc2` returns a `list`
#' \enumerate{
#' \item **`[[StabStats]]`** The first level of this list contains the outputs of `ERAg::StabCalc` for each Practice x Outcome ( x EU if Inc.Prod==T) combination present in `Data`
#'  \item **`[[StabStats.Test]]`** This is the equivalent of the **`[[Tests]]`** `data.table` output by `ERAg::StabCalc`
#'  \item **`[[StabStats.Tab]]`** This is the equivalent of the **`[[Coefs]]`** `data.table` output by `ERAg::StabCalc`
#'  \item **`[[StabStats.Test2]]`** This is the equivalent of the **`[[Tests2]]`** `data.table` output by `ERAg::StabCalc`
#'  \item **`[[StabList]]`** A `list` of the input `Data` `data.table` split by Practice and Outcome ( and EU if Inc.Prod==T)
#'  }
#' @export
#' @import data.table
#' @importFrom pbapply pblapply
#' @importFrom data.table dcast rbindlist
#' @importFrom snow makeCluster clusterEvalQ registerDoSNOW stopCluster parLapply
#' @importFrom doSnow registerDoSNOW
StabCalc2<-function(Data,
                    Do.Weight=T,
                    Weight.by.Study=T,
                    Rm.Out=T,
                    Transform=T,
                    DoRandom=T,
                    Control=list(optimizer="optim",optmethod="Nelder-Mead",maxit=10000),
                    Responses=c("lnRR","lnVR","lnCVR"),
                    Use.acv=F,
                    Inc.Prod=F,
                    Cores=1){

  Data<-data.table(Data)

  # Ensure at least 3 years long
  Data<-Data[nryears>=3]

  if(Inc.Prod){
    Data<-split(Data,list(Data$Outcome,Data$Practice,Data$EU))
  }else{
    Data<-split(Data,list(Data$Outcome,Data$Practice))
  }

  Data<-Data[unlist(lapply(Data,nrow))>=3]

  if(Cores==1){
    StabStats<-pbapply::pblapply(1:length(Data),FUN=function(i){
      DATA<-Data[[i]]
      X<-ERAg::StabCalc(Data=DATA,
                        Do.Weight=Do.Weight,
                        Weight.by.Study=Weight.by.Study,
                        Rm.Out=Rm.Out,
                        Transform=Transform,
                        DoRandom=DoRandom,
                        Control=Control,
                        Responses=Responses,
                        Use.acv=Use.acv)
    })
  }else{
    cl<-snow::makeCluster(Cores)
    snow::clusterEvalQ(cl, list(library(ERAg),library(data.table),library(broom),library(MASS),
                                library(sfsmisc),library(metafor),library(Matrix),library(plyr)))
    snow::clusterExport(cl,list("Data","Do.Weight","Weight.by.Study","Rm.Out","Transform","DoRandom","Control","Responses",
                                "Use.acv"),envir=environment())
    doSNOW::registerDoSNOW(cl)

    StabStats<-snow::parLapply(cl,1:length(Data),fun=function(i){
      DATA<-Data[[i]]
      X<-StabCalc(Data=DATA,
                  Do.Weight=Do.Weight,
                  Weight.by.Study=Weight.by.Study,
                  Rm.Out=Rm.Out,
                  Transform=Transform,
                  DoRandom=DoRandom,
                  Control=Control,
                  Responses=Responses,
                  Use.acv=Use.acv)
    })

    snow::stopCluster(cl)

  }

  StabStats.Tab<-rbindlist(lapply(StabStats,"[[","Coefs"))
  StabStats.Tab[,N.Seq:=round(mean(N.Seq),0)][,N.Obs:=round(mean(N.Obs),0)][,N.Studies:=round(mean(N.Studies),0)]
  if(Inc.Prod){
    StabStats.Tab<-dcast(StabStats.Tab,Model+Robust+N.Studies+N.Seq+N.Obs+Practice+Practice.Code+Outcome+EU~Response,value.var = c("Mean","SE","Z.val","CI.low","CI.high","P.Vals","Mean.Jen","CI.low.Jen","CI.high.Jen"))
  }else{
    StabStats.Tab<-dcast(StabStats.Tab,Model+Robust+N.Studies+N.Seq+N.Obs+Practice+Practice.Code+Outcome~Response,value.var = c("Mean","SE","Z.val","CI.low","CI.high","P.Vals","Mean.Jen","CI.low.Jen","CI.high.Jen"))
  }

  if(Inc.Prod){
    StabStats.Test<-dcast(rbindlist(lapply(StabStats,"[[","Tests")),Practice+Practice.Code+Outcome+EU+Robust+N.Obs+N.Studies~Variable+Coefficient,value.var=c("Estimate","Std. Error","t value","Pr(>|t|)","PSymbol","Mean.Jen","CI.low.Jen","CI.high.Jen"))
  }else{
    StabStats.Test<-dcast(rbindlist(lapply(StabStats,"[[","Tests")),Practice+Practice.Code+Outcome+Robust+N.Obs+N.Studies~Variable+Coefficient,value.var=c("Estimate","Std. Error","t value","Pr(>|t|)","PSymbol","Mean.Jen","CI.low.Jen","CI.high.Jen"))
  }
  StabStats.Test[,Prac.x.Out:=paste0(Outcome,".",Practice)]

  StabStats.Test2<-rbindlist(lapply(StabStats,"[[","Tests2"))


  return(list(StabStats=StabStats,StabStats.Test=StabStats.Test,StabStats.Tab=StabStats.Tab,StabStats.Test2=StabStats.Test2,StabList=Data))
}

