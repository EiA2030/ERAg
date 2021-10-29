## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load packages, eval=F,echo=T---------------------------------------------
#    if(!require("pacman", character.only = TRUE)){
#      install.packages("pacman",dependencies = T)
#    }
#  
#    required.packages <- c("ERAg","data.table","miceadds","dplyr")
#    p_load(char=required.packages,install = T,character.only = T)
#  

## ----load packages real, eval=T,echo=F,include=F------------------------------
  require(ERAg)
  require(data.table)

## ----create dirs, eval=F,echo=T-----------------------------------------------
#  # Create a directory for the climate analysis
#  ClimDir<-"ERA Climate Analysis/422814143322303020152010130-3POWCHIFALSE0.6/"
#  if(!dir.exists(ClimDir)){
#    dir.create(ClimDir)
#  }

## ----Load ClimateA, eval=F,echo=T---------------------------------------------
#  ClimateA<-load.Rdata2("ClimStatsA.RData",path=ClimDir)
#  
#  names(ClimateA)

## ----Load ERA, eval=F,echo=T--------------------------------------------------
#  ERA<-load.Rdata2("Data.RData",path=AnalysisDir)

## ----load soils,echo=T,eval=T-------------------------------------------------
Soils<-ERA_ISDA
Soils[,1:3]

## ----soils show Variable names,echo=T,eval=T----------------------------------
Soils[,unique(Variable)]

## ----soils update Variable names,echo=T,eval=T--------------------------------
Soils[grepl("0..200cm",Variable),Depth.Upper:=0
      ][grepl("0..200cm",Variable),Depth.Lower:=200
        ][,Variable:=gsub("_m_30m_0..200cm","",Variable)]

Soils[grepl("0..20cm",Variable),Depth.Upper:=0
      ][grepl("0..20cm",Variable),Depth.Lower:=20
        ][,Variable:=gsub("_m_30m_0..20cm","",Variable)]

Soils[grepl("20..50cm",Variable),Depth.Upper:=20
      ][grepl("20..50cm",Variable),Depth.Lower:=50
        ][,Variable:=gsub("_m_30m_20..50cm","",Variable)]

Soils[,unique(Variable)]


## ----soil weights, echo=T,eval=T----------------------------------------------
Soils[,Weight:=Depth.Lower-Depth.Upper]

Soils<-unique(Soils[,list(Median=weighted.mean(Median,Weight),
                          Mean=weighted.mean(Mean,Weight),
                          SD=weighted.mean(SD,Weight),
                          Mode=Mode[Weight==max(Weight)]),
                    by=list(Variable,Site.Key)])

## ----soils dcast, echo=T,eval=T-----------------------------------------------
colnames(Soils)

Soils<-dcast(Soils,Site.Key~Variable,value.var = c("Median","Mean","SD","Mode"))

colnames(Soils)

## ----merge MCode, echo=T,eval=F-----------------------------------------------
#  Clim.Data<-ClimateA[["Observed"]]
#  
#  # Concatenate location, crop codes and planting date fields
#  Clim.Data[,MCode:=paste(ID,EU,PD.Used,sep="|")]
#  ERA[,MCode:=paste(Site.Key,EU,P.Date.Merge,sep="|")]

## ----merge MCode LT, echo=T,eval=F--------------------------------------------
#  LTAvg.Clim.Data<-ClimateA[["LongTerm"]][["LT.Clim.Avg"]]
#  
#  # Concatenate location, crop codes and season fields
#  LTAvg.Clim.Data[,MCode.LT:=paste(ID,EU,Season)]
#  Clim.Data[,MCode.LT:=paste(ID,EU,Season)]
#  

## ----merge lapply, echo=T,eval=F----------------------------------------------
#  Merge Climate & Soil Data with ERA ####
#  ERA.Clim.Soil<-lapply(Clim.Data[,unique(W.Name)],FUN=function(X){
#  
#    # Subset seasonal climate data to calculation window X
#    CData<-Clim.Data[W.Name==X]
#  
#    # Subset long-term climate averages to calculation window X
#    LT.Clim.Data<-LTAvg.Clim.Data[W.Name==X]
#  
#    # Subset long-term climate averages for mean and median variables
#    LT.Clim.Data.Mean<-LT.Clim.Data[Variable=="Mean"]
#    LT.Clim.Data.Median<-LT.Clim.Data[Variable=="Median"]
#  
#    # Male an object containing the column names of climate variables for which we wish to calculate deviance
#    Cols<-colnames(CData[,!c("Tmax.sd","Tmean.sd","PD.Used","W.Start","W.End","W.Name","ID","M.Year","Season","MCode","MCode.LT","EU","ETo.NA")])
#  
#    # Subtract long term average climate from the observed seasonal climate
#    Mean.Diff<-CData[,..Cols]-LT.Clim.Data.Mean[match(CData[,MCode.LT],LT.Clim.Data.Mean[,MCode.LT])][,..Cols]
#    Median.Diff<-CData[,..Cols]-LT.Clim.Data.Median[match(CData[,MCode.LT],LT.Clim.Data.Median[,MCode.LT])][,..Cols]
#  
#    # Rename columns to reflect they are now deviance from long-term mean or median values
#    colnames(Mean.Diff)<-paste0(colnames(Mean.Diff),".Dev.Mean")
#    colnames(Median.Diff)<-paste0(colnames(Median.Diff),".Dev.Median")
#  
#    # Join seasonal climate with deviance from long-term average tables
#    CData<-cbind(CData,Mean.Diff,Median.Diff)
#  
#    # Match soils data to ERA using Site.Key field
#    SoilData<-Soils[match(ERA[,Site.Key],Site.Key)]
#  
#    # Join ERA, Climate and Soils datasets removing any duplicated columns
#    cbind(ERA,
#          CData[match(ERA$MCode,MCode),!c("MCode","EU","ID")],
#          SoilData[,!"Site.Key"])
#    })
#  
#  # Name list levels
#  names(ERA.Clim.Soil)<-unique(Clim.Data$W.Name)

## ----load vignette data, echo=F,eval=T,include=F------------------------------
ERA.Clim.Soil<-ERA.Clim.Soil.Vignette

## ----show results, echo=T,eval=T----------------------------------------------
names(ERA.Clim.Soil)

ERA.Clim.Soil[["EcoCrop"]][!is.na(ETo.sum)][c(1,30)]


