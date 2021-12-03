#' Prepare Data for Outcome Stability Analysis
#'
#' This function prepares data for use with the `StabCalc` function. It calculates relative, absolute and adjusted coefficients of yield variation
#' for unique temporal sequences of at least three years.
#'
#' The data supplied needs to have `Practice` and `Outcome` fields as generated using the `ERAg::StandColNames` function.
#'
#' The units of measurement (the `Unit` field) for the supplied data
#' should contain values of only "Mg/ha", although there is limited functionality to harmonize other units ("kg/ha", "kg/acre", "Kg/fed", "Mg/fed", and
#' "g/m2") to "Mg/ha".
#'
#' The Adjusted Coefficient of Variation (acv) is calculated using the metan::acv function.
#'
#' @param Data  a `data.frame` or `data.table` of ERA data with column names standardized using the `StandColNames` function.
#' @param OutCodes A vector of outcome codes to consider in the analysis. Default = `101` (Crop Yield).
#' @return a `data.table` including fields of the data supplied, subset to unique temporal sequences with the following columns appended:
#' * `UID` integer, a unique identifier for a pairing of control and experimental treatments (CT and ET) and their outcome that is used to match observations across time.
#' * `UID.C` integer, a unique identifier for a CT that is used to match observations across time.
#' * `UID.T` integer, a unique identifier for an ET that is used to match observations across time.
#' * `yieldcont` numeric, the mean yield of the CT.
#' * `minyieldcont` numeric, the minimum yield of the CT.
#' * `yieldexp`  numeric, the mean yield of the ET.
#' * `sdcont` numeric, the standard deviation of the CT.
#' * `varcont`  numeric, the variance of the CT.
#' * `sdexp` numeric, the standard deviation of the ET.
#' * `varexp` numeric, the variance of the ET.
#' * `cvcont` numeric, the coefficient of variation for the CT as `sdcont/yieldcont`.
#' * `acvcont` numeric, the adjusted coefficient of variation for the CT.
#' * `cvexp` numeric, the coefficient of variation for the ET as `sdexp/yieldexp`.
#' * `acvexp`  numeric, the adjusted coefficient of variation for the ET.
#' * `yieldratio` numeric, the ratio of ET:CT yields as `yieldexp/yieldcont`.
#' * `sdratio` numeric, the absolute variance ratio of ET:CT yields as `sdexp/sdcont`.
#' * `cvratio` numeric, the relative variance ratio of ET:CT yields as `cvexp/cvcont`.
#' * `acvratio` numeric, the adjusted relative variance ratio of ET:CT yields as `acvexp/acvcont`.
#' * `Rep`  integer, the number of replicates for the temporal sequence.
#' * `nryears` integer, the number of yield observations in a temporal sequence.
#' * `cluster_cont` integer, within a study (`Code` field) temporal sequences that share the same value have the same CT across multiple ETs.
#' * `UID.C.Len` integer, the number of temporal sequences that have the same `cluster_cont` value within a study.
#' * `cluster_exp` integer, within a study (`Code` field) temporal sequences that share the same value have the same ET across multiple CTs.
#' * `UID.T.Len` integer, the number of temporal sequences that have the same `cluster_xp` value within a study.
#' * `UID1` integer, a unique identifier for each UID within outcomes (UID is unique across all the data).
#' * `N.Obs` integer, the number of temporal sequences for a UID (this should equal one).
#' * `N.Obs.Study` integer, number of temporal sequences contribude by a study (a unique `Code` field value) for a Practice x Outcome combination.
#' * `Weight.Study` numeric, a weighting value calculated as `(Rep^2/(2xRep))/N.Obs.Study`.
#' * `Weight` numeric, a weighting value calculated as `(Rep^2/(2xRep))/N.Obs`.
#' @export
#' @import data.table
#' @importFrom metan acv
PrepareStabData<-function(Data,OutCodes=101){
  Data<-data.table(Data)
  Data<-Data[Data$Outcode %in% OutCodes]

  # Reject any temporally aggregated yields
  Data<-Data[!nchar(M.Year)>6]

  # Ensure units are harmonized
  Kg.Units<-c("kg/ha","kg/ha/yr")
  Data[Units %in% Kg.Units,MeanC:=MeanC/1000
  ][Units %in% Kg.Units,MeanT:=MeanT/1000
  ][Units %in% Kg.Units,Units:="Mg/ha"]

  Data[Units=="kg/acre",MeanC:=(MeanC/1000)/2.47105
  ][Units=="kg/acre",MeanT:=(MeanT/1000)/2.47105
  ][Units=="kg/acre",Units:="Mg/ha"]

  Data[Units=="kg/fed",MeanC:=(MeanC/1000)*0.41682658
  ][Units=="kg/fed",MeanT:=(MeanT/1000)*0.41682658
  ][Units=="kg/fed",Units:="Mg/ha"]

  Data[Units=="Mg/fed",MeanC:=MeanC*0.41682658
  ][Units=="Mg/fed",MeanT:=MeanT*0.41682658
  ][Units=="Mg/fed",Units:="Mg/ha"]

  g.Units<-c("g/m2")
  Data[Units %in% g.Units,MeanC:=(MeanC/10^6)*10^4
  ][Units %in% g.Units,MeanT:=(MeanT/10^6)*10^4
  ][Units %in% g.Units,Units:="Mg/ha"]

  g.Units<-c("g Dry Matter/m2")
  Data[Units %in% g.Units,MeanC:=(MeanC/10^6)*10^4
  ][Units %in% g.Units,MeanT:=(MeanT/10^6)*10^4
  ][Units %in% g.Units,Units:="Mg Dry Matter/ha"]

  Data<-Data[Units %in% c("Mg/ha","Mg Dry Matter/ha")]

  # Code below can be used to interrogate any strange yield values
  # View(unique(Data[MeanT>10 & Product %in% c("Maize","Wheat","Rice")  & Outcode==101,list(Code,Units,MeanC,MeanT,Product)]))
  # ERA.Compiled[Code=="NJ0059" & Outcode==101 & (MeanT>10|MeanC>10),list(Units,MeanC,MeanT,Product.Simple,DataLoc,Author,M.Year)]

  # Create unique identity for observation removing temporal elements
  Cols<-c("Outcome","Practice","plist","Practice.Base","base.list","Practice.Code","Code","ID","Site.ID","EU","T.Descrip","C.Descrip","TID","CID","T.NI","T.NO","C.NI","C.NO","Tree","Variety","Diversity","Rep","Units")
  suppressWarnings(Data[,UID:=apply(Data[,..Cols],1,FUN=function(X){paste(X,collapse="")})])

  # Calculate statistics per unique time-series
  Data[,yieldcont:=mean(MeanC),by=UID
  ][,minyieldcont:=min(MeanC),by=UID
  ][,varcont:=var(MeanC),by=UID
  ][,sdcont:=sd(MeanC),by=UID
  ][,cvcont:=sdcont/yieldcont
  ][,yieldexp:=mean(MeanT),by=UID
  ][,varexp:=var(MeanT),by=UID
  ][,sdexp:=sd(MeanT),by=UID
  ][,cvexp:=sdexp/yieldexp
  ][,yieldratio:=yieldexp/yieldcont
  ][,sdratio:=sdexp/sdcont
  ][,cvratio:=cvexp/cvcont
  ][,Rep:=mean(Rep),by=UID
  ][,nryears:=length(unique(M.Year)),by=UID]  # Calculate number of growing seasons

  # Remove any infinite or NA values (generate where there are zero values in the data)
  Data<-Data[!(is.na(cvratio)|is.na(sdratio)|is.infinite(cvratio)|is.infinite(sdratio)|is.infinite(yieldratio)|is.infinite(yieldratio))]

  # Subset data to at least 3 years of data
  Data<-Data[nryears>=3]

  # Temp code to deal with errors that generate strange variance
  #Data<-Data[!(varexp>1000|varcont>1000)]

  # Create unique identity for control removing temporal elements
  Cols.C<-c("Outcome","base.list","Code","ID","Site.ID","EU","C.Descrip","CID","C.NI","C.NO","yieldcont")
  suppressWarnings(Data[,UID.C:=apply(Data[,..Cols.C],1,FUN=function(X){paste(X,collapse="")})])

  # Create unique identity for practice removing temporal elements
  Cols.T<-c("Outcome","Practice","base.list","Code","ID","Site.ID","EU","T.Descrip","TID","T.NI","T.NO","Tree","Variety","Diversity","Rep","yieldexp")
  suppressWarnings(Data[,UID.T:=apply(Data[,..Cols.T],1,FUN=function(X){paste(X,collapse="")})])


  # Calculate Adjusted Coefficent of Variation
  X<-unique(Data[,list(UID.C,yieldcont,varcont,Outcome)])
  suppressWarnings(X[,acv:=metan::acv(yieldcont,varcont)[,"acv"],by=Outcome])
  Data[,acvcont:=X[match(Data[,UID.C],UID.C),acv]]

  Y<-unique(Data[,list(UID.T,yieldexp,varexp,Outcome)])
  suppressWarnings(Y[,acv:=metan::acv(yieldexp,varexp)[,"acv"],by=Outcome])
  Data[,acvexp:=Y[match(Data[,UID.T],UID.T),acv]]

  Data[,acvratio:=acvexp/acvcont]

  Cols<-c("Code","ID","Site.ID","EU","T.Descrip","C.Descrip","TID","CID","T.NI","T.NO","C.NI","C.NO","Tree","Variety","Diversity","Practice","Practice.Base","Practice.Code","plist","base.list","Outcome","UID","UID.C","UID.T","yieldcont","minyieldcont","yieldexp","sdcont","varcont","sdexp","varexp","cvcont","acvcont","cvexp","acvexp","yieldratio","sdratio","cvratio","acvratio","Rep","nryears")

  suppressWarnings( Data<-unique(Data[,..Cols]))

  # Make UIDs more interpretable within a study & add cluster identities within studies
  # If a control is only used once in a study then set to NA
  Data[,UID.T:=as.numeric(as.factor(UID.T))
  ][,UID.C:=as.numeric(as.factor(UID.C))
  ][,cluster_cont:=as.numeric(as.factor(UID.C)),by=list(Practice,Outcome)   # Control clusters are where the same control is compared to multiple treatments
  ][,UID.C.Len:=.N,by=list(cluster_cont,Practice,Outcome)   # If a control is only used once in a study then set to NA & values within a cluster must match
  ][UID.C.Len==1,cluster_cont:=NA
  ][,cluster_exp:=as.numeric(as.factor(UID.T)),by=list(Practice,Outcome)   # Experimental clusters are where multiple controls are compared to the same treatment
  ][,UID.T.Len:=.N,by=list(cluster_exp,Practice,Outcome)
  ][UID.T.Len==1,cluster_exp:=NA   # If a treatment is only used once in a study then set to NA
  ][,UID1:=as.numeric(as.factor(UID)),by=list(Practice,Outcome)
  ][,UID.T:=as.numeric(as.factor(UID.T))
  ][,UID.C:=as.numeric(as.factor(UID.C))
  ][,UID:=as.numeric(as.factor(UID))
  ][,N.Obs:=.N,by=UID
  ][,N.Obs.Study:=.N,by=list(Practice,Code,Outcome)
  ][,Weight.Study:=(Rep^2/(2*Rep))/N.Obs.Study  # Add weighting
  ][,Weight:=(Rep^2/(2*Rep))/N.Obs]


  return(Data)

}
