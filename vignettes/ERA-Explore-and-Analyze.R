## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages,include=F,eval=T,echo=T-----------------------------------------
require(ERAg)
require(tidyr)
require(treemap)
require(spatstat)

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
ERAg::ERAHexPlot(Data=ERA.Pits,Showpoints = "Yes",Point.Col = "black",Mid = "yellow",
  High = "red",)

100*(ERA.Pits[,table(Country)] / nrow(ERA.Pits)) %>% round(digits=3)

## ----WH find papers-----------------------------------------------------------
ERA.Pits[,list(Code,Journal,Author,DOI)] %>%  unique

## ----WH find papers 2---------------------------------------------------------
ERA.Pits[1:5,list(DOI,PrName,Out.SubInd,MeanT,MeanC,Units,DataLoc)] 

