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
#' the number of #' observations contributed by a study to the dataset weights each study equally.   As such, outcome ratios are weighted according to:
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
#'    * *95% confidence intervals* use the \link[stats]{confint} function with `method = "Wald"`
#'    * *weighted variance* uses the \link[Hmisc]{wtd.stats} `wtd.var` function
#'    * *weighted quantiles* use the \link[spatstat.geom]{weighted.median} (`weighted.quantile`) function with `probs=seq(0,1,0.25)`
#'
#' 5) Response ratios are back-transformed and converted to % change with and without a correction for the Jensen inequality.
#' The correction applied is as per \href{https://www.biorxiv.org/content/10.1101/179358v1}{Tandini & Mehrabi 2017}.
#'
#' 6) When `Fast = FALSE` where minimum data requirements are met linear-mixed effects or linear model is applied to the data to generate means, standard errors and variance.
#'     * Linear mixed effects models use \link[lmer]{lme4} where outcomes from a grouping variable combination are from at least three sites of which two must have at
#' least three observations. The model is weighted and includes a random intercept for site (`lmer(Value~1 + (1|Site),weights=Weights)`).
#'     * If the minimum data requirements for the lmer are not met then a linear model with weights is applied (`lm(Value~1,weights=Weights)`) if there are at least 5 outcome observations for the grouping variable combination.
#'     * If the minimum data requirements for the lm are not met no test is applied to the outcome values.
#'
#' Also note that any groupings of data specified in the Aggregate.By parameter for which values of MeanC and MeanT are identical are removed from the dataset before analysis.
#'
#' @param Data A preapred ERA dataset (see PrepareERA function)
#' @param rmOut Logical T/F. If TRUE extreme outliers are detected and removed for each combination of grouping variables.
#' @param Aggregate.By Column names for grouping variables. Statistics will be compiled for each combination of these variables.
#' @param ROUND Integer value for the number of decimal places numeric columns in the output dataset should be rounded to.
#' @param Fast Logical T/F. If `FALSE` then lmer and lm models are used to estimate means, errors and significance if sufficient data exist.
#' @param UseAllCores Logical T/F. If `TRUE` then data.table uses the maximum number of cores available for parallel processing.
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
#' * `RR.CIlow` = lower 95% confidence interval of RR
#' * `RR.CIhigh` = upper 95% confidence interval of RR
#' * `RR.var` = weighted variance of RR
#' * `RR.Quantiles05` = weighted quantiles of the RR
#' * `PC.Shapiro.Sig` = P-value from a Shapiro-Wilk test of PC
#' * `PC` = weighted mean of PC
#' * `PC` = weighted median of PC
#' * `PC.se` = weighted standard error of PC
#' * `PC.CIlow` = lower 95% confidence interval of PC
#' * `PC.CIhigh` = upper 95% confidence interval of PC
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
#' * `RR.pc.jen.CIlow` = lower 95% confidence interval of % change based on RR with correction for Jensen inequality
#' * `RR.pc.jen.CIhigh` = upper 95% standard error confidence interval of % change based on RR with correction for Jensen inequality
#
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
#' @import data.table
#' @importFrom data.table setDTthreads
#' @importFrom stats weighted.mean shapiro.test
#' @importFrom spatstat.geom weighted.median weighted.quantile
#' @importFrom diagis weighted_se
#' @importFrom Hmisc wtd.var
#' @importFrom lmerTest lmer
ERAAnalyze<-function(Data,rmOut=T,Aggregate.By,ROUND=5,Fast=F,UseAllCores=F){

  if(UseAllCores){
  data.table::setDTthreads(threads = 0)
  }
  options(scipen=999)

  Data<-suppressWarnings(Data[,yi:=log(MeanT/MeanC)][!(is.na(yi)|is.infinite(yi))])

  # Remove Outliers
  if(rmOut){

    Outliers<-unlist(Data[,R:=1:nrow(Data)
    ][,list(Outliers=list(R[ERAg::OutCalc(yi)])), by=Aggregate.By
    ][,Outliers])

    Data<-Data[!Outliers]
  }

  # Remove any data where MeanC and MeanT are identical (creates Error in asMethod(object) : not a positive definite matrix)

  Data<-Data[,Identical:=all(MeanT==MeanC),by=Aggregate.By][Identical==F][,Identical:=NULL]


  # Estimate treatment effect size ####

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
              Sites=length(unique(Site.Key)),
              RR.Shapiro.Sig=round(FunShap(log(MeanT/MeanC)),ROUND),
              RR=stats::weighted.mean(log(MeanT/MeanC),Weight.Study,na.rm=T),
              RR.median=spatstat.geom::weighted.median(log(MeanT/MeanC),Weight.Study,na.rm = T),
              RR.se=diagis::weighted_se(log(MeanT/MeanC), Weight.Study, na.rm=T),
              RR.CIlow=suppressWarnings(confint(lm(log(MeanT/MeanC)~1,weights=Weight.Study))[1]),
              RR.CIhigh=suppressWarnings(confint(lm(log(MeanT/MeanC)~1,weights=Weight.Study))[2]),
              RR.var=suppressWarnings(abs(Hmisc::wtd.var(log(MeanT/MeanC),Weight.Study,na.rm=T))),
              RR.Quantiles0.25=if(.N==1){as.character(NA)}else{if(length(unique(MeanC/MeanT))==1){as.character(NA)}else{paste(round(spatstat.geom::weighted.quantile(log(MeanT/MeanC),Weight.Study,probs=seq(0,1,0.25),na.rm=T),ROUND),collapse="|")}},
              PC.Shapiro.Sig=round(FunShap(MeanT/MeanC),ROUND),
              PC=stats::weighted.mean(MeanT/MeanC,Weight.Study,na.rm=T),
              PC.median=spatstat.geom::weighted.median(MeanT/MeanC,Weight.Study,na.rm = T),
              PC.se=diagis::weighted_se(MeanT/MeanC, Weight.Study, na.rm=T),
              PC.CIlow=suppressWarnings(confint(lm(MeanT/MeanC~1,weights=Weight.Study))[1]),
              PC.CIhigh=suppressWarnings(confint(lm(MeanT/MeanC~1,weights=Weight.Study))[2]),
              PC.var=suppressWarnings(abs(Hmisc::wtd.var(MeanT/MeanC,Weight.Study,na.rm=T))),
              PC.Quantiles0.25=if(.N==1){as.character(NA)}else{if(length(unique(MeanC/MeanT))==1){as.character(NA)}else{paste(round(spatstat.geom::weighted.quantile(MeanT/MeanC,Weight.Study,probs=seq(0,1,0.25),na.rm=T),ROUND),collapse="|")}},
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
      ][,RR.pc.jen.high:=round(100*exp(RR+RR.se+RR.var/2)-100,ROUND)
      ][,RR.pc.jen.CIlow:=round(100*exp(RR.CIlow+RR.var/2)-100,ROUND)
      ][,RR.pc.jen.CIhigh:=round(100*exp(RR.CIhigh+RR.var/2)-100,ROUND)
      ],

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
              RR.CIlow=suppressWarnings(confint(lm(log(MeanT/MeanC)~1,weights=Weight.Study))[1]),
              RR.CIhigh=suppressWarnings(confint(lm(log(MeanT/MeanC)~1,weights=Weight.Study))[1]),
              RR.Quantiles0.25=if(.N==1){as.character(NA)}else{if(length(unique(MeanC/MeanT))==1){as.character(NA)}else{paste(round(spatstat.geom::weighted.quantile(log(MeanT/MeanC),Weight.Study,probs=seq(0,1,0.25),na.rm=T),ROUND),collapse="|")}},
              PC.Shapiro.Sig=FunShap(MeanT/MeanC),
              PC=stats::weighted.mean(MeanT/MeanC,Weight.Study,na.rm=T),
              PC.median=spatstat.geom::weighted.median(x=MeanT/MeanC,w=Weight.Study,na.rm = T),
              PC.se=diagis::weighted_se(MeanT/MeanC, Weight.Study, na.rm=T),
              PC.CIlow=suppressWarnings(confint(lm(MeanT/MeanC~1,weights=Weight.Study))[1]),
              PC.CIhigh=suppressWarnings(confint(lm(MeanT/MeanC~1,weights=Weight.Study))[1]),
              PC.var=suppressWarnings(abs(Hmisc::wtd.var(MeanT/MeanC,Weight.Study,na.rm=T))),
              PC.Quantiles0.25=if(.N==1){as.character(NA)}else{if(length(unique(MeanC/MeanT))==1){as.character(NA)}else{paste(round(spatstat.geom::weighted.quantile(MeanT/MeanC,Weight.Study,probs=seq(0,1,0.25),na.rm=T),ROUND),collapse="|")}},
              Units=if(length(unique(Units))==1){unique(Units)}else{"Multiple"},
              # To run the lmer the requirement of three or more sites of which two must have at least three observations must be met
              # If not sufficient data for random-effects model run a t-test if >5 observations
              RR.lmer=list(if(length(unique(ID))>2 & sum(table(ID)>2)>=2 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
                suppressWarnings(lmerTest::lmer(log(MeanT/MeanC)~1 + (1|ID),weights=Weight.Study))
              }else{
                if(length(ID)>5 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
                  suppressWarnings(lm(log(MeanT/MeanC)~1,weights=Weight.Study))
                }else{NA}
              }),
              PC.lmer=list(if(length(unique(ID))>2 & sum(table(ID)>2)>=2 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
                suppressWarnings(lmerTest::lmer(MeanT/MeanC~1 + (1|ID),weights=Weight.Study))
              }else{
                if(length(ID)>5 & !sum(Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"))>0){
                  suppressWarnings(lm(MeanT/MeanC~1,weights=Weight.Study))
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

    RR.CI<-lapply(1:nrow(ANALYSED.Data),FUN=function(i){
      X<-ANALYSED.Data[i,RR.lmer][[1]]
      suppressWarnings(
        if(class(X) %in% c("lm","lmerModLmerTest")){
          X<-round(confint(X,method="Wald"),ROUND)
          if(nrow(X)>1){
            X[3,]
          }else{
            X
          }
        }else{
          c(NA,NA)
        }
      )
    })

    XRR.CIhigh<-unlist(lapply(RR.CI,"[",2))
    XRR.CIlow<-unlist(lapply(RR.CI,"[",1))

    PC.Models<-do.call("rbind",lapply(1:nrow(ANALYSED.Data),FUN=function(i){
      round(ExtractModel(ANALYSED.Data[i,PC.lmer][[1]],"PC"),ROUND)
    }))

    if(all(is.na(RR.Models))){
      colnames(RR.Models)<-c("RR.Estimate","RR.Std. Error","RR.t value","RR.Pr(>|t|)","RR.Sigma2")
      RR.Models<-data.table(RR.Models)
    }

    if(all(is.na(PC.Models))){
      colnames(PC.Models)<-c("PC.Estimate","PC.Std. Error","PC.t value","PC.Pr(>|t|)","PC.Sigma2")
      PC.Models<-data.table(PC.Models)

    }

    ANALYSED.Data<-cbind(ANALYSED.Data,RR.Models,PC.Models)

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
    ][is.na(RR.pc.jen.high) & !RR.se==0,RR.pc.jen:=round(100*exp(RR+RR.se+RR.var/2)-100,ROUND)
    ][,RR.CIlow:=XRR.CIlow
    ][,RR.CIhigh:=XRR.CIhigh
    ][,RR.pc.jen.CIlow:=round(100*exp(RR.CIlow+RR.Sigma2/2)-100,ROUND)
    ][,RR.pc.jen.CIhigh:=round(100*exp(RR.CIhigh+RR.Sigma2/2)-100,ROUND)]


    ANALYSED.Data[!is.na(PC.Estimate),PC:=PC.Estimate
    ][!is.na(`PC.Std. Error`),PC.se:=`PC.Std. Error`
    ][,PC.Estimate:=NULL
    ][,`PC.Std. Error`:=NULL
    ][,PC.lmer:=unlist(lapply(PC.lmer,class))
    ][,PC.pc.se.low:=round(100*(PC-PC.se)-100,ROUND)
    ][,PC.pc:=round(100*PC-100,ROUND)
    ][,PC.pc.se.high:=round(100*(PC+PC.se)-100,ROUND)]

    ANALYSED.Data[,RR.lmer:=NULL]
    data.table::setnames(ANALYSED.Data,"PC.lmer","Model")
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
