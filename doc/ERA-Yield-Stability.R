## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,include=F,eval=T,echo=T--------------------------------------------
require(ERAg)
require(broom)
require(ggplot2)
require(metafor)
require(pbapply)
require(plyr)
require(sfsmisc)
require(MASS)

## ----Access ERA, echo=T-------------------------------------------------------
knitr::kable(head(ERA.Compiled[,1:8], 5))

## ----Show Practice Codes, echo=T----------------------------------------------
knitr::kable(head(PracticeCodes[,1:6], 5))

## ----Show ERA Concepts, echo=T------------------------------------------------
ERAConcepts

## ----Run StandColName Function, echo=T----------------------------------------
StabData<-StandColNames(Data=ERA.Compiled,
              PLevel="P",
              OLevel="SI",
              EULevel="P"
              )

## ----Run Subset on Product, echo=T,eval=F-------------------------------------
#  StabData<-StabData[Product.Subtype=="Cereals"]

## ----Show Outcome Codes, echo=T-----------------------------------------------
data.table(OutcomeCodes)[grep("Crop Yield",Subindicator),list(Subindicator,Code)]

## ----Prepare Stability Data, echo=T-------------------------------------------
StabData<-PrepareStabData(Data=StabData,OutCodes=101)

## ----Subset Stability Data, echo=T,eval=F-------------------------------------
#  StabData<-StabData[nryears>=4]
#  

## ----Stability calc, echo=T, eval=T,include=F---------------------------------
StabData<-StabCalc2(Data=StabData,
          Do.Weight=T,
          Weight.by.Study=T,
          Rm.Out=T,
          Transform=T,
          Control=list(optimizer="optim",optmethod="Nelder-Mead",maxit=10000),
          Responses=c("lnRR","lnVR","lnCVR"))

## ----Stability Plots, echo =T-------------------------------------------------
StabPlots<-ERAStabPlot(Data=StabData,Robust=F,Intercept=F)

## ----Example Plots, echo =T,fig.width=7,fig.height=6.5,warning=F--------------
plot(StabPlots$`Crop Yield.Agroforestry Pruning`$lnVR)
plot(StabPlots$`Crop Yield.Agroforestry Pruning`$lnCVR)

