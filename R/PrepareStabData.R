#' Prepare Data for Outcome Stability Analysis
#'
#' This function prepares data for use with the `StabCalc` function
#'
#' @param Data  a `data.frame` or `data.table` of ERA data with column names standardized using the `StandColNames` function.
#' @param OutCodes A vector of outcome codes to consider in the analysis. Default = `101` (Crop Yield).
#' @return a `data.table` of the supplied data with appended columns:
#' * *column descriptions to be added*
#' @export
# Function to calculate stats for each temporal sequence x unique treatment x outcome x site
PrepareStabData<-function(Data,OutCodes=101){
  Data<-data.table(Data)
  Data<-Data[Data$Outcode %in% OutCodes]

  # Create unique identity for observation removing temporal elements
  Cols<-c("Outcome","Practice","Practice.Base","Practice.Code","Code","ID","Site.ID","EU","T.Descrip","C.Descrip","TID","CID","T.NI","T.NO","C.NI","C.NO","Tree","Variety","Diversity","Rep")
  suppressWarnings(Data[,UID:=apply(Data[,..Cols],1,FUN=function(X){paste(X,collapse="")})])

  # Calculate statistics per unique time-series
  Data[,yieldcont:=mean(MeanC),by=UID
  ][,minyieldcont:=min(MeanC),by=UID
  ][,sdcont:=sd(MeanC),by=UID
  ][,cvcont:=sdcont/yieldcont
  ][,yieldexp:=mean(MeanT),by=UID
  ][,sdexp:=sd(MeanT),by=UID
  ][,cvexp:=sdexp/yieldexp
  ][,yieldratio:=yieldexp/yieldcont
  ][,sdratio:=sdexp/sdcont
  ][,cvratio:=cvexp/cvcont
  ][,Rep:=mean(Rep),by=UID
  ][,nryears:=length(unique(M.Year)),by=UID]  # Calculate number of growing seasons

  # Create unique identity for control removing temporal elements
  Cols.C<-c("Outcome","base.list","Code","ID","Site.ID","EU","C.Descrip","CID","C.NI","C.NO","yieldcont")
  suppressWarnings(Data[,UID.C:=apply(Data[,..Cols.C],1,FUN=function(X){paste(X,collapse="")})])

  # Create unique identity for practice removing temporal elements
  Cols.T<-c("Outcome","Practice","base.list","Code","ID","Site.ID","EU","T.Descrip","TID","T.NI","T.NO","Tree","Variety","Diversity","Rep","yieldexp")
  suppressWarnings(Data[,UID.T:=apply(Data[,..Cols.T],1,FUN=function(X){paste(X,collapse="")})])

  Cols<-c("Code","ID","Site.ID","EU","T.Descrip","C.Descrip","TID","CID","T.NI","T.NO","C.NI","C.NO","Tree","Variety","Diversity","Practice","Practice.Base",
          "Practice.Code","plist","Outcome","UID","UID.C","UID.T","yieldcont","minyieldcont","yieldexp","sdcont","sdexp","cvcont","cvexp",
          "yieldratio","sdratio","cvratio","Rep","nryears")

  suppressWarnings( Data<-unique(Data[,..Cols]))
  # Remove any infinite or NA values (generate where there are zero values in the data)
  Data<-Data[!(is.na(cvratio)|is.na(sdratio)|is.infinite(cvratio)|is.infinite(sdratio)|is.infinite(yieldratio)|is.infinite(yieldratio))]

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
  ][,Weight:=(Rep^2/(2*Rep))/N.Obs
  ]

  return(Data)

}
