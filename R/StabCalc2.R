#' Calculate Outcome Stability
#'
#' `StabCalc2`runs splits the `data.table` output by `PrepareStabData` into a list using the `Outcome` & `Practice` fields then applies `StabCalc`
#' to each element of the list.
#'
#' @param Data *To be described*
#' @param Do.Weight *To be described*
#' @param Weight.by.Study *To be described*
#' @param Rm.Out *To be described*
#' @param Transform *To be described*
#' @param Control *To be described*
#' @param Responses *To be described*
#' @param Use.acv logical T/F. If T scale-adjusted coefficient of variation, acv, is substituted for the coefficient of variation (cv).
#' @return `StabCalc2` returns a `data.table`
#' Output fields:
#'
#' * * To be described*
#' @export
StabCalc2<-function(Data,
                    Do.Weight=T,
                    Weight.by.Study=T,
                    Rm.Out=T,
                    Transform=T,
                    DoRandom=T,
                    Control=list(optimizer="optim",optmethod="Nelder-Mead",maxit=10000),
                    Responses=c("lnRR","lnVR","lnCVR"),
                    Use.acv=F){

  Data<-data.table(Data)

  # MYOs at least 3 years long (redundant now this is incorporated into PrepareStabData?)
  Data<-Data[nryears>=3]
  Data<-Data[,N:=.N,by=list(Practice,Outcome)]

  # At least 3 MYOs
  Data<-Data[N>=3]

  Data<-split(Data,list(Data$Outcome,Data$Practice))


  StabStats<-pblapply(1:length(Data),FUN=function(i){
    print(i)
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
  StabStats.Tab<-rbindlist(lapply(StabStats,"[[","Coefs"))
  StabStats.Tab[,N.Seq:=round(mean(N.Seq),0)][,N.Obs:=round(mean(N.Obs),0)][,N.Studies:=round(mean(N.Studies),0)]
  StabStats.Tab<-dcast(StabStats.Tab,Model+Robust+N.Studies+N.Seq+N.Obs+Practice+Practice.Code+Outcome~Response,value.var = c("Mean","SE","Z.val","CI.low","CI.high","P.Vals","Mean.Jen","CI.low.Jen","CI.high.Jen"))

  StabStats.Test<-dcast(rbindlist(lapply(StabStats,"[[","Tests")),Practice+Practice.Code+Outcome+Robust+N.Obs+N.Studies~Variable+Coefficient,value.var=c("Estimate","Std. Error","t value","Pr(>|t|)","PSymbol","Mean.Jen","CI.low.Jen","CI.high.Jen"))
  StabStats.Test[,Prac.x.Out:=paste0(Outcome,".",Practice)]

  StabStats.Test2<-rbindlist(lapply(StabStats,"[[","Tests2"))


  return(list(StabStats=StabStats,StabStats.Test=StabStats.Test,StabStats.Tab=StabStats.Tab,StabStats.Test2=StabStats.Test2,StabList=Data))
}


