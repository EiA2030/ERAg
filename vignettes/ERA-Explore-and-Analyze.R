## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages,include=F,eval=T,echo=T-----------------------------------------
require(ERAg)
require(tidyr)
require(treemap)

## ----Combo numbers, echo =TRUE------------------------------------------------
# No. combination practices
nrow(ERA.Compiled[grepl("-",SubPrName)])

# No. solo practices
nrow(ERA.Compiled[!grepl("-",SubPrName)])

## ----Splitting Combinations, echo =T------------------------------------------

# There are many subpractice combinations in ERA
ERA.Compiled[grepl("-",SubPrName),length(unique(SubPrName))]
ERA.Compiled[,unique(SubPrName)][1:10]

# Lets split these
ERA.Compiled.Split<-ERAComboSplit(Data=ERA.Compiled)

# The dataset gets longer
dim(ERA.Compiled)
dim(ERA.Compiled.Split)

# Suffixed .Combo columns contain split names and codes
grep(".Combo",colnames(ERA.Compiled.Split),value=T)
# Combine codes have gone in the .Combo columns
ERA.Compiled.Split[grepl("-",SubPrName.Combo),length(unique(SubPrName.Combo))]
# Only single values are presnt
ERA.Compiled.Split[,unique(SubPrName.Combo)][1:10]

## ----Treemap, echo =T,fig.width=7,fig.asp=0.66--------------------------------

# First we count the number of studies per level of the practice heirarchy (see PracticeCodes object for more information on the practice heirarchy)
ERA.Compiled.Split.Pracs<-ERA.Compiled.Split[,list(N.Studies=length(unique(Code))),by=list(SubPrName.Combo,PrName.Combo,Theme.Combo)]

head(ERA.Compiled.Split.Pracs)

# Visualize with the treemap function
treemap::treemap(ERA.Compiled.Split.Pracs,
      index=c("Theme.Combo","PrName.Combo"),
      vSize="N.Studies",
      type="index",
      palette = "Paired",
      title="Number of Studies reporting ERA practices")

## ----WH Explore Theme,echo=T--------------------------------------------------
PracticeCodes<-data.table(PracticeCodes)

PracticeCodes[,unique(Theme)]


## ----WH Explore Practice,echo=T-----------------------------------------------
PracticeCodes[Theme=="Water Management",unique(Practice)]

## ----WH Explore Subpractice,echo=T--------------------------------------------
# Note that we are using the Subpractice.S field, this is the field used to generate the names in the ERA.Compiled$SubPrName and ERA.Compiled$SubPrName.Base fields
PracticeCodes[Practice=="Water Harvesting",unique(Subpractice.S)]

## ----WH Subpractice Definition,echo=T-----------------------------------------
FocalPractices<-c("Planting Basins","Zai Pits")
PracticeCodes[Subpractice %in% FocalPractices,Definition]

## ----WH tabulate practices,echo=T---------------------------------------------
  (PitPracs<-ERA.Compiled[grepl(paste(FocalPractices,collapse = "|"),SubPrName),SubPrName] %>%
  table %>%
  sort(decreasing = T))

## ----WH Subset,echo=T---------------------------------------------------------
  PitPracs<-PitPracs[PitPracs>=50] %>% names
  ERA.Pits<-ERA.Compiled[SubPrName %in% PitPracs] 

## ----WH visualize outcomes,fig.width=7,fig.asp=0.66,echo=T--------------------
ERA.Pits.Out<-ERA.Pits[,list(N.Studies=length(unique(Code))),by=list(SubPrName,Out.SubInd)]

# Visualize with the treemap function
treemap::treemap(ERA.Pits.Out,
      index=c("Out.SubInd"),
      vSize="N.Studies",
      type="index",
      palette = "Paired",
      title="Outcomes present for pit-based water harvesting practices")

## ----WH visualize products,fig.width=7,fig.asp=0.66,echo=T--------------------
ERA.Pits.Crops<-ERA.Pits[,list(N.Studies=length(unique(Code))),by=list(Product.Simple)]

# Visualize with the treemap function
treemap::treemap(ERA.Pits.Crops,
      index=c("Product.Simple"),
      vSize="N.Studies",
      type="index",
      palette = "Paired",
      title="Crops present for pit-based water harvesting practices")

## ----WH map data,echo=T,fig.width=7,fig.asp=0.66------------------------------
ERAgON::ERAHexPlot(Data=ERA.Pits,Showpoints = "Yes",Point.Col = "black",Mid = "yellow",
  High = "red",)

100*(ERA.Pits[,table(Country)] / nrow(ERA.Pits)) %>% round(digits=3)

## ----WH find papers-----------------------------------------------------------
ERA.Pits[,list(Code,Journal,Author,DOI)] %>%  unique

## ----WH find papers 2---------------------------------------------------------
ERA.Pits[1:5,list(DOI,PrName,Out.SubInd,MeanT,MeanC,Units,DataLoc)] 

## ----WH Prepare the data,echo=T-----------------------------------------------
ERA.Pits.Prep<-ERAg::PrepareERA(Data = ERA.Pits,Perc.Neg = 0.5,RmNeg = T)

# Have we lost any data?
dim(ERA.Pits)
dim(ERA.Pits.Prep)

## ----WH analyze,echo=T,warning=FALSE------------------------------------------
Analysis1<-ERAg::ERAAnalyze(Data=ERA.Pits.Prep,
            # rmOut: Outliers will be removed
            rmOut = TRUE, 
            # Aggregate.By: Data are analyzed as subsets of these fields 
            Aggregate.By = c("Out.SubInd","SubPrName"), 
            # Fast: Tests will be applied to the data if FALSE
            Fast=FALSE) 

## ----WH analyzed fields ,echo=T-----------------------------------------------
colnames(Analysis1)

## ----WH analyzed fields 1,echo=T----------------------------------------------
Analysis1[,list(Out.SubInd, SubPrName, Observations, Studies, Sites)] %>%  head

## -----------------------------------------------------------------------------
Analysis1<-Analysis1[Studies>=3]
Analysis1[,list(Out.SubInd, SubPrName, Observations, Studies, Sites)]

## ----WH - RRs, echo=T---------------------------------------------------------
Analysis1[,list(Out.SubInd,SubPrName,RR.Shapiro.Sig, RR, RR.median, RR.var, RR.se, RR.Quantiles0.25)] %>% head

## ----WH - RRs 2, echo=T-------------------------------------------------------
Analysis1[,list(Out.SubInd,SubPrName,Model, `RR`,`RR.se`,`RR.t value`, `RR.Pr(>|t|)`, RR.Sigma2)] %>% head

## ----WH - RRs 3, echo=T-------------------------------------------------------
Analysis1[,list(Out.SubInd,SubPrName, RR.pc.se.low, RR.pc, RR.pc.se.high, RR.pc.jen.low, RR.pc.jen.high)] %>% head

## ----WH - Comparison 1, echo=T------------------------------------------------
# Add percentage change results based on median
Analysis1[,PC.pc.median:=100*(PC.median-1)]
Analysis1[,RR.pc.median:=100*(RR.median-1)]

Analysis1[,list(Out.SubInd,SubPrName,Observations,RR.pc.jen,RR.pc.median,PC.pc,PC.pc.median)]

## ----WH - Comparison 2, echo=T------------------------------------------------
Analysis1[,list(Out.SubInd,SubPrName, RR.Shapiro.Sig, PC.Shapiro.Sig)]

## ----WH - Comparison 3, echo =T,,fig.width=7,fig.asp=0.66,echo=T--------------
RR<-ERA.Pits.Prep[Out.SubInd=="Soil Moisture"	& SubPrName=="MinTill-Planting Basins",yi]
hist(RR, prob=T)
curve(dnorm(x, mean=mean(RR), sd=sqrt(var(RR))), col="darkblue", lwd=2, add=TRUE, yaxt="n")

Proportion<-ERA.Pits.Prep[Out.SubInd=="Soil Moisture"	& SubPrName=="MinTill-Planting Basins",MeanT/MeanC]
hist(Proportion, prob=T)
curve(dnorm(x, mean=mean(Proportion), sd=sqrt(var(Proportion))), col="darkblue", lwd=2, add=TRUE, yaxt="n")


## ----WH - PC, echo=T----------------------------------------------------------
Con<-c(1000,1500,2000) # Control outcomes
Exp<-c(1100,500,1200) # Experimental outcomes

# ***Experimental outcome is numerator***
Exp/Con
mean(Exp/Con)
# % change estimated from mean of ratios
100*(mean(Exp/Con)-1) 
# % change estimated from mean of response ratios
100*(exp(mean(log(Exp/Con)))-1) 

# ***Control outcome is numerator***
Con/Exp
mean(Con/Exp)
# % change estimated from mean of ratios
100*(mean(Con/Exp)-1) 
# % change estimated from mean of response ratios
100*(exp(mean(log(Con/Exp)))-1) 

