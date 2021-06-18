#' Analyze Outcome Ratios
#'
#' This function analyzes outcome ratios in the ERA dataset for each combination of grouping variables as specified by column names in the `Aggregate.By` parameter. We suggest
#' applying the `ERA.Prepare` function to data before using with this function.
#'
#' Several actions are or can be applied by this function:
#'
#' 1) Outlier Removal: Outliers are defined using an extreme outliers method where values above or below $3*IQR$ (interquartile range) are removed. The ERA outcome variables
#' analyzed by this function are ratios between am experimental treatment and control outcome and should be approximately normally distributed. When the control approaches
#' zero (e.g. yield collapse) this skews the distribution of the outcome ratio producing extremely high values tending to infinity and requiring outlier removal.
#' The use of outcome ratios, whilst necessary to standardize outcomes between studies, means this approach is inappropriate to study nil outcomes (e.g. total crop yield failure),
#' a binomial approach would be better for such instances. Outlier removal is optional and enabled if the rmOut parameter is set to TRUE (default).
#'
#' 2) Weighting: Within-study variance measures for mean outcomes are infrequently reported in agricultural literature, so traditional meta-analytic approaches cannot be applied
#' to most ERA outcomes. Therefore individual observations are up-weighted by replication and down-weighted by the number of observations submitted from the same study (colname =
#' Code) for each combination of grouping variables. Studies with more replications are likely to produce less variable information than studies with fewer. Controlling for
#' the number of #' observations contributed by a study to the dataset weights each study equally.  As such, outcome ratios are weighted according to:
#' `Weighting = ((RepsE * RepsC)/(RepsE)+(RepsC))/(Ns)`  where `Rep` is the number of replications for `RepC` the control and `RepE` the experimental
#' treatment, and `Ns` is the total number of observations contributed to the overall dataset by the study to which the observation belongs.
#'
#' 3) Test of Normality: A Shapiro-Wilk test ( \link[stats]{shapiro.test}) is applied to raw and log-transformed outcome ratios for each combination of grouping variables. This can be used
#' to judge whether values based on mean proportional change, mean response ratio or median proportional change should be used to evaluate practice performance.
#'
#' 4) Statistics calculated (in all cases na.rm=T):
#'    * *weighted means* use the \link[stats]{weighted.mean} function
#'    * *weighted medians* use the \link[spatstat.geom]{weighted.median} function
#'    * *weighted standard errors* use the \link[diagis]{weighted_se} function
#'    * *weighted variance* uses the \link[Hmisc]{wtd.var} function
#'    * *weighted quantiles* use the \link[spatstat.geom]{weighted.median} (`weighted.quantile`) function with `probs=seq(0,1,0.25)`
#'
#' 5) Response ratios are back-transformed and converted to % change using with and without a correction for the Jensen inequality.
#' The correction applied is as per \href{https://www.biorxiv.org/content/10.1101/179358v1}{Tandini & Mehrabi 2017}.
#'
#' 6) When `Fast = FALSE` where minimum data requirements are met linear-mixed effects or linear model is applied to the data to generate means, standard errors and variance.
#'     * Linear mixed effects models use \link[lmer]{lme4} where outcomes from a grouping variable combination are from at least three sites of which two must have at
#' least three observations. The model is weighted and includes a random intercept for site (`lmer(Value~1 + (1|Site),weights=Weights)`).
#'     * If the minimum data requirements for the lmer are not met then a linear model with weights is applied (`lm(Value~1,weights=Weights)`) if there are at least 5 outcome observations for the grouping variable combination.
#'     * If the minimum data requirements for the lm are not met no test is applied to the outcome values.
#'
#' @param Data A preapred ERA dataset (see PrepareERA function)
#' @param rmOut Logical T/F. If TRUE extreme outliers are dectected and removed for each combination of grouping variables.
#' @param Aggregate.By Column names for grouping variables. Statistics will be compiled for each combination of these variables.
#' @param ROUND Integer value for the number of decimal places numeric columns in the output dataset should be rounded to.
#' @param Fast Logical T/F. If `FALSE` then lmer and lm models are used to estimate means, errors and significance if sufficient data exist.
#' @return
#' A data.table of response ratios and percentage change values, each row representing a combination of the grouping variables specified in the Aggregrate.By parameter. `RR` = outcome response ratio $log(MeanT/MeanC)$, `PC` = outcome proportional change $MeanT/MeanC$.
#'
#' Output columns when `Fast` is `TRUE`:
#' * `Observations` = no. rows of data
#' * `Studies` = no. studies (publications)
#' * `Sites` = no. of geographic locations
#' * `RR.Shapiro.Sig` = P-value from a Shapiro-Wilk test of RR
#' * `RR` = weighted mean of RR
#' * `RR` = weighted median of RR
#' * `RR.se` = weighted standard error of RR
#' * `RR.var` = weighted variance of RR
#' * `RR.Quantiles05` = weighted quantiles of the RR
#' * `PC.Shapiro.Sig` = P-value from a Shapiro-Wilk test of PC
#' * `PC` = weighted mean of PC
#' * `PC` = weighted median of PC
#' * `PC.se` = weighted standard error of PC
#' * `PC.var` = weighted variance of PC
#' * `PC.Quantiles05` = weighted quantiles of the PC
#' * `PC.pc` = percent change based on PC (`100 x PC - 100`)
#' * `PC.pc.se.low` = lower standard error confidence interval of % change based on PC
#' * `PC.pc.se.high` = upper standard error confidence interval of % change based on PC
#' * `RR.pc` = % change based on RR (`100 x exp(RR) - 100`)
#' * `RR.pc.se.low` = lower standard error confidence interval of % change based on RR
#' * `RR.pc.se.high` = upper standard error confidence interval of % change based on RR
#' * `RR.pc.jen` = % change based on RR with correction for Jensen inequality (`100 x exp(RR+RR.var/2) - 100`)
#' * `RR.pc.jen.low` = lower standard error confidence interval of % change based on RR with correction for Jensen inequality
#' * `RR.pc.jen.high` = upper standard error confidence interval of % change based on RR with correction for Jensen inequality
#'
#' Where all units are indentical for the grouping variables (row) then the following columns will have values (else they are NA):
#' * `Units` = the unit of recording for an outcome (e.g. kg/ha)
#' * `MeanT.Obs` = number of experimental treatment observations
#' * `MeanT` = weighted mean of experimental treatment outcome values
#' * `MeanT.se` = weighted standard error of experimental treatment outcome values
#' * `MeanC.Obs` = number of control treatment observations
#' * `MeanC` = weighted mean of control treatment outcome values
#' * `MeanC.se` = weighted standard error of contol treatment outcome values
#'
#' When `Fast = TRUE` means, standard errors and confidence intervals are replaced by estimates from, in order of preference, lmer then lm models, if the model minimum data
#' requirements are met. Percentage change data is then calculated from the updated estimates.
#'
#' Additional columns when `Fast = FALSE`:
#'  * `Model` = type of model that was applied to data, `NA` = no model was applied.
#'  * `RR.t.value` = t statistic from RR model
#'  * `RR.Pr(>|t|)` = probability that the outcome is not equal to zero from RR model
#'  * `RR.Sigma2` = RR model sigma2
#'  * `PC.t.value` = t statistic from PC model
#'  * `PC.Pr(>|t|)` = probability that the outcome is not equal to zero from PC model
#'  * `PC.Sigma2` = PC model sigma2
#' @export
ERAAnalyze2<-function(Data,rmOut=T,Aggregate.By,ROUND=5,Fast=F){
  options(scipen=999)

  Data<-Data[!(is.na(log(MeanT/MeanC))|is.infinite(log(MeanT/MeanC)))]

  # Create function that balances the geometry of negative and positive changes by deciding numerator/denominator
  # based on whether T>C or T<C
  ERA.Change<-function(Exp,Con){
    Vals<- 100*(Exp/Con-1)
    Vals[Exp>Con]<- -100*(Con[Exp>Con]/Exp[Exp>Con]-1)
    return(Vals)
  }

  # Remove Outliers
  if(rmOut){

    suppressWarnings(Data[,yi:=log(MeanT/MeanC)])


    Outliers<-unlist(Data[,R:=1:nrow(Data)
    ][,list(Outliers=list(R[OutCalc(yi)])), by=Aggregate.By
    ][,Outliers])

    Data<-Data[!Outliers]
  }

  # Estimate treatment effect size ####

  # In output tables:
  # RR = response ratio
  # RR.sd = standard deviation of response ratio
  # pc = percent change
  # pc.sd = standard deviation of percent change


  FunShap<-function(X){
    tryCatch(stats::shapiro.test(X)$p.val,error=function(cond) {as.numeric(NA)})
  }

  if(Fast){


    #Faster, less accurate: Estimates weighted means and errors only ####
    Weight.Group<-unique(c("Code",Aggregate.By))

    ANALYSED.Data<-cbind(
      Data[,N.Obs.Study:=.N,by=Weight.Group # Recalculate Weightings by study within obervations grouping
      ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study # Recalculate Weightings
      ][,list(Observations=.N,
              Studies=length(unique(Code)),
              Sites=length(unique(ID)),
              RR.Shapiro.Sig=round(FunShap(log(MeanT/MeanC)),ROUND),
              RR=stats::weighted.mean(log(MeanT/MeanC),Weight.Study,na.rm=T),
              RR.median=spatstat.geom::weighted.median(log(MeanT/MeanC),Weight.Study,na.rm = T),
              RR.se=diagis::weighted_se(log(MeanT/MeanC), Weight.Study, na.rm=T),
              RR.var=suppressWarnings(abs(Hmisc::wtd.var(log(MeanT/MeanC),Weight.Study,na.rm=T))),
              RR.Quantiles0.25=paste(round(spatstat.geom::weighted.quantile(log(MeanT/MeanC),Weight.Study,probs=seq(0,1,0.25),na.rm=T),ROUND),collapse="|"),
              PC.Shapiro.Sig=round(FunShap(MeanT/MeanC),ROUND),
              PC=stats::weighted.mean(MeanT/MeanC,Weight.Study,na.rm=T),
              PC.median=spatstat.geom::weighted.median(MeanT/MeanC,Weight.Study,na.rm = T),
              PC.se=diagis::weighted_se(MeanT/MeanC, Weight.Study, na.rm=T),
              PC.var=suppressWarnings(abs(Hmisc::wtd.var(MeanT/MeanC,Weight.Study,na.rm=T))),
              PC.Quantiles0.25=paste(round(spatstat.geom::weighted.quantile(MeanT/MeanC,Weight.Study,probs=seq(0,1,0.25),na.rm=T),ROUND),collapse="|"),
              ERA.Change=stats::weighted.mean(ERA.Change(MeanT,MeanC),Weight.Study,na.rm=T),
              ERA.Change.median=spatstat.geom::weighted.median(ERA.Change(MeanT,MeanC),Weight.Study,na.rm=T),
              ERA.Change.se=diagis::weighted_se(ERA.Change(MeanT,MeanC),Weight.Study,na.rm=T),
              ERA.Change.var=suppressWarnings(abs(Hmisc::wtd.var(ERA.Change(MeanT,MeanC),Weight.Study,na.rm=T))),
              ERA.Change.Quantiles0.25=paste(round(spatstat.geom::weighted.quantile(ERA.Change(MeanT,MeanC),Weight.Study,probs=seq(0,1,0.25),na.rm=T),ROUND),collapse="|"),
              Units=if(length(unique(Units))==1){unique(Units)}else{"Multiple"}
      ),by=Aggregate.By
      ][,PC.pc:=round(100*PC-100,ROUND)
      ][,PC.pc.se.low:=round(100*(PC-PC.se)-100,ROUND)
      ][,PC.pc.se.high:=round(100*(PC+PC.se)-100,ROUND)
      ][,RR.pc:=round(100*exp(RR)-100,ROUND)
      ][,RR.pc.se.low:=round(100*exp(RR-RR.se)-100,ROUND)
      ][,RR.pc.se.high:=round(100*exp(RR+RR.se)-100,ROUND)
      ][,RR.pc.jen:=round(100*exp(RR+RR.var/2)-100,ROUND)
      ][,RR.pc.jen.low:=round(100*exp(RR-RR.se+RR.var/2)-100,ROUND)
      ][,RR.pc.jen.high:=round(100*exp(RR+RR.se+RR.var/2)-100,ROUND)],

      Data[,list(MeanT=mean(MeanT),Rep=mean(Rep,na.rm=T)),by=c(Weight.Group,"TID")
      ][,N.Obs.Study:=.N,by=Weight.Group # Recalculate Weightings by study within observations grouping
      ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study # Recalculate Weightings
      ][,list(MeanT.Obs=.N,
              MeanT=round(stats::weighted.mean(MeanT,Weight.Study,na.rm=T),ROUND),
              MeanT.se=suppressWarnings(abs(Hmisc::wtd.var(MeanT,Weight.Study,na.rm=T)))
      ),
      by=Aggregate.By  # this
      ][,MeanT.se:=round(MeanT.se/MeanT.Obs^0.5,ROUND)
      ][,c("MeanT.Obs","MeanT","MeanT.se")],

      Data[,list(MeanC=mean(MeanC),Rep=mean(Rep,na.rm=T)),by=c(Weight.Group,"TID")
      ][,N.Obs.Study:=.N,by=Weight.Group # Recalculate Weightings by study within obervations grouping
      ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study # Recalculate Weightings
      ][,list(MeanC.Obs=.N,
              MeanC=round(stats::weighted.mean(MeanC,Weight.Study,na.rm=T),ROUND),
              MeanC.se=suppressWarnings(abs(Hmisc::wtd.var(MeanC,Weight.Study,na.rm=T)))
      ),
      by=Aggregate.By  # this
      ][,MeanC.se:=round(MeanC.se/MeanC.Obs^0.5,ROUND)
      ][,c("MeanC.Obs","MeanC","MeanC.se")]
    )
    ANALYSED.Data[Units=="Multiple",c("MeanT.Obs","MeanT","MeanT.se","MeanC.Obs","MeanC","MeanC.se")]<-NA

  }else{

    # 2.4b) Slower, more accurate: Code that estimate parameters using LMs/LMMs and generates signifance values ####
    # I suggest that we pre-make tables for different grouping and then read these in rather than recalculating the data on the fly.

    # t value = test statistic from lm or lmm model, where NA there were insufficient data to run a test
    # Pr(>|t|) = significance of test statistic

    ExtractModel<-function(X,Prefix){
      if(!is.na(Prefix)){
        Names<- paste0(Prefix,".",c("Estimate","Std. Error","t value","Pr(>|t|)","Sigma2"))
      }else{
        Names<- c("Estimate","Std. Error","t value","Pr(>|t|)","Sigma2")
      }

      if(!class(X)=="logical"){
        if(class(X)[1]=="lmerModLmerTest"){
          X<-c(summary(X)$coefficients[-3],sigma2=summary(X)$sigma^2)
          names(X)<-Names
        }else{
          X<-c(summary(X)$coefficients,sigma2=summary(X)$sigma^2)
          names(X)<-Names
        }
      }else{
        X<-rep(NA,5)
      }
      return(X)
    }

    Weight.Group<-unique(c("Code",Aggregate.By))


    ANALYSED.Data<-cbind(
      Data[,N.Obs.Study:=.N,by=Weight.Group # Recalculate Weightings
      ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study # Recalculate Weightings
      ][,list(Observations=.N,
              Studies=length(unique(Code)),
              Sites=length(unique(ID)),
              RR.Shapiro.Sig=FunShap(log(MeanT/MeanC)),
              RR=stats::weighted.mean(log(MeanT/MeanC),Weight.Study,na.rm=T),
              RR.median=spatstat.geom::weighted.median(x=log(MeanT/MeanC),w=Weight.Study,na.rm = T),
              RR.var=suppressWarnings(abs(Hmisc::wtd.var(log(MeanT/MeanC),Weight.Study,na.rm=T))),
              RR.se=diagis::weighted_se(log(MeanT/MeanC), Weight.Study, na.rm=T),
              RR.Quantiles0.25=paste(round(spatstat.geom::weighted.quantile(x=log(MeanT/MeanC),w=Weight.Study,probs=seq(0,1,0.25),na.rm=T),ROUND),collapse="|"),
              PC.Shapiro.Sig=FunShap(MeanT/MeanC),
              PC=stats::weighted.mean(MeanT/MeanC,Weight.Study,na.rm=T),
              PC.median=spatstat.geom::weighted.median(x=MeanT/MeanC,w=Weight.Study,na.rm = T),
              PC.se=diagis::weighted_se(MeanT/MeanC, Weight.Study, na.rm=T),
              PC.var=suppressWarnings(abs(Hmisc::wtd.var(MeanT/MeanC,Weight.Study,na.rm=T))),
              PC.Quantiles0.25=paste(round(spatstat.geom::weighted.quantile(x=MeanT/MeanC,w=Weight.Study,probs=seq(0,1,0.25),na.rm=T),ROUND),collapse="|"),
              ERA.Change=stats::weighted.mean(ERA.Change(MeanT,MeanC),Weight.Study,na.rm=T),
              ERA.Change.median=spatstat.geom::weighted.median(ERA.Change(MeanT,MeanC),Weight.Study,na.rm=T),
              ERA.Change.se=diagis::weighted_se(ERA.Change(MeanT,MeanC),Weight.Study,na.rm=T),
              ERA.Change.var=suppressWarnings(abs(Hmisc::wtd.var(ERA.Change(MeanT,MeanC),Weight.Study,na.rm=T))),
              ERA.Change.Quantiles0.25=paste(round(spatstat.geom::weighted.quantile(ERA.Change(MeanT,MeanC),Weight.Study,probs=seq(0,1,0.25),na.rm=T),ROUND),collapse="|"),
              Units=if(length(unique(Units))==1){unique(Units)}else{"Multiple"},
              # To run the lmer the requirement of three or more sites of which two must have at least three observations must be met
              # If not sufficient data for random-effects model run a t-test if >5 observations
              RR.lmer=list(if(length(unique(ID))>2 & sum(table(ID)>2)>=2 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
                lmerTest::lmer(log(MeanT/MeanC)~1 + (1|ID),weights=Weight.Study)
              }else{
                if(length(ID)>5 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
                  lm(log(MeanT/MeanC)~1,weights=Weight.Study)
                }else{NA}
              }),
              PC.lmer=list(if(length(unique(ID))>2 & sum(table(ID)>2)>=2 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
                lmerTest::lmer(MeanT/MeanC~1 + (1|ID),weights=Weight.Study)
              }else{
                if(length(ID)>5 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
                  lm(MeanT/MeanC~1,weights=Weight.Study)
                }else{NA}
              }),
              ERA.Change.lmer=list(if(length(unique(ID))>2 & sum(table(ID)>2)>=2 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
                lmerTest::lmer(ERA.Change(MeanT,MeanC)~1 + (1|ID),weights=Weight.Study)
              }else{
                if(length(ID)>5 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
                  lm(ERA.Change(MeanT,MeanC)~1,weights=Weight.Study)
                }else{NA}
              })
      ),
      by=Aggregate.By],

      Data[,list(MeanT=mean(MeanT),
                 Rep=mean(Rep,na.rm=T),
                 Units=if(length(unique(Units))==1){unique(Units)}else{"Multiple"}),by=c(Weight.Group,"TID")
      ][,N.Obs.Study:=.N,by=Weight.Group # Recalculate Weightings by study within obervations grouping
      ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study # Recalculate Weightings
      ][,list(MeanT.Obs=.N,
              MeanT=round(stats::weighted.mean(MeanT,Weight.Study,na.rm=T),ROUND),
              MeanT.se=suppressWarnings(abs(Hmisc::wtd.var(MeanT,Weight.Study,na.rm=T)))
      ),
      by=Aggregate.By  # this
      ][,MeanT.se:=round(MeanT.se/MeanT.Obs^0.5,ROUND)
      ][,c("MeanT.Obs","MeanT","MeanT.se")],

      Data[,list(MeanC=mean(MeanC),
                 Rep=mean(Rep,na.rm=T),
                 Units=if(length(unique(Units))==1){unique(Units)}else{"Multiple"}),by=c(Weight.Group,"CID")
      ][,N.Obs.Study:=.N,by=Weight.Group # Recalculate Weightings by study within obervations grouping
      ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study # Recalculate Weightings
      ][,list(MeanC.Obs=.N,
              MeanC=round(stats::weighted.mean(MeanC,Weight.Study,na.rm=T),ROUND),
              MeanC.se=suppressWarnings(abs(Hmisc::wtd.var(MeanC,Weight.Study,na.rm=T)))
      ),
      by=Aggregate.By  # this
      ][,MeanC.se:=round(MeanC.se/MeanC.Obs^0.5,ROUND)
      ][,c("MeanC.Obs","MeanC","MeanC.se")]
    )

    ANALYSED.Data[Units=="Multiple",c("MeanT.Obs","MeanT","MeanT.se","MeanC.Obs","MeanC","MeanC.se")]<-NA

    RR.Models<-do.call("rbind",lapply(1:nrow(ANALYSED.Data),FUN=function(i){
      round(ExtractModel(ANALYSED.Data[i,RR.lmer][[1]],"RR"),ROUND)
    }))

    PC.Models<-do.call("rbind",lapply(1:nrow(ANALYSED.Data),FUN=function(i){
      round(ExtractModel(ANALYSED.Data[i,PC.lmer][[1]],"PC"),ROUND)
    }))

    ERA.Change.Models<-do.call("rbind",lapply(1:nrow(ANALYSED.Data),FUN=function(i){
      round(ExtractModel(ANALYSED.Data[i,ERA.Change.lmer][[1]],"ERA.Change"),ROUND)
    }))

    ANALYSED.Data<-cbind(ANALYSED.Data,RR.Models,PC.Models,ERA.Change.Models)

    ANALYSED.Data[!is.na(RR.Estimate),RR:=RR.Estimate
    ][!is.na(`RR.Std. Error`),RR.se:=`RR.Std. Error`
    ][,RR.Estimate:=NULL
    ][,`RR.Std. Error`:=NULL
    ][,RR.lmer:=unlist(lapply(RR.lmer,class))
    ][,RR.pc.se.low:=round(100*exp(RR-RR.se)-100,ROUND)
    ][,RR.pc:=round(100*exp(RR)-100,ROUND)
    ][,RR.pc.se.high:=round(100*exp(RR+RR.se)-100,ROUND)
    ][,RR.pc.jen.low:=round(100*exp(RR-RR.se+RR.Sigma2/2)-100,ROUND)
    ][is.na(RR.pc.jen.low) & !RR.se==0,RR.pc.jen:=round(100*exp(RR-RR.se+RR.var/2)-100,ROUND)
    ][,RR.pc.jen:=round(100*exp(RR+RR.Sigma2/2)-100,ROUND)
    ][is.na(RR.pc.jen) & !RR.se==0,RR.pc.jen:=round(100*exp(RR+RR.var/2)-100,ROUND)
    ][,RR.pc.jen.high:=round(100*exp(RR+RR.se+RR.Sigma2/2)-100,ROUND)
    ][is.na(RR.pc.jen.high) & !RR.se==0,RR.pc.jen:=round(100*exp(RR+RR.se+RR.var/2)-100,ROUND)]

    ANALYSED.Data[!is.na(PC.Estimate),PC:=PC.Estimate
    ][!is.na(`PC.Std. Error`),PC.se:=`PC.Std. Error`
    ][,PC.Estimate:=NULL
    ][,`PC.Std. Error`:=NULL
    ][,PC.lmer:=unlist(lapply(PC.lmer,class))
    ][,PC.pc.se.low:=round(100*(PC-PC.se)-100,ROUND)
    ][,PC.pc:=round(100*PC-100,ROUND)
    ][,PC.pc.se.high:=round(100*(PC+PC.se)-100,ROUND)]

    ANALYSED.Data[!is.na(ERA.Change.Estimate),PC:=ERA.Change.Estimate
    ][!is.na(`ERA.Change.Std. Error`),ERA.Change.se:=`ERA.Change.Std. Error`
    ][,ERA.Change.Estimate:=NULL
    ][,`ERA.Change.Std. Error`:=NULL
    ][,ERA.Change.lmer:=unlist(lapply(ERA.Change.lmer,class))
    ][,ERA.Change.ERA.Change.se.low:=round(100*(PC-ERA.Change.se)-100,ROUND)
    ][,ERA.Change.pc:=round(100*PC-100,ROUND)
    ][,ERA.Change.ERA.Change.se.high:=round(100*(PC+ERA.Change.se)-100,ROUND)]

    ANALYSED.Data[,RR.lmer:=NULL]
    setnames(ANALYSED.Data,"PC.lmer","Model")
    ANALYSED.Data[Model=="logical",Model:=NA]

  }

  ANALYSED.Data[,RR:=round(RR,ROUND)
  ][,RR.var:=round(RR.var,ROUND)
  ][,RR.se:=round(RR.se,ROUND)
  ][,PC:=round(PC,ROUND)
  ][,PC.var:=round(PC.var,ROUND)
  ][,PC.se:=round(PC.se,ROUND)
  ][,PC.Shapiro.Sig:=round(PC.Shapiro.Sig,ROUND)
  ][,RR.Shapiro.Sig:=round(RR.Shapiro.Sig,ROUND)]

  return(ANALYSED.Data)
}

