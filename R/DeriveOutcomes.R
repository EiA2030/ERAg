#' Derive ERA outcomes from existing data
#'
#' This function identifies exactly colocated outcomes (same experiment, treatment, location and time) ERA data that can be combined
#' to create additional outcome observations, generates these additional outcomes and appends them to the supplied Data.
#'
#' The function currently generates additional outcomes for:
#' 1) Net Returns = gross returns - total costs
#' 2) Gross Margin = gross returns - variable costs
#' 3) Benefit Cost Ratio (GRTC) =  gross returns/total costs
#' 4) Benefit Cost Ratio (NRTC) =  net returns/total costs
#' 5) Benefit Cost Ratio (GRVC) =  gross returns/variable costs
#' 6) Benefit Cost Ratio (NRVC) =  net returns/variable costs
#'
#' Derived net returns are appended to `Data` before benefit cost ratio outcomes are derived.
#'
#' Note that many of the outcomes derived from the data can be negative therefore be careful not to exclude these outcomes when applying the `ERAg::PrepareERA`
#' function (the `Perc.Neg` parameter could be adjusted to retain negative outcomes).
#'
#' @param Data An ERA data.table (e.g. `ERAg::ERA.Compiled`).
#' @param RmPartial Logical `T/F`. If `T` partial economic outcomes are excluded from the process.
#' @param DoBCR_VC Logical `T/F`. If `T` benefit cost ratios calculated using variable costs are also included in calculations as per 5) & 6) in
#' decription.
#' @return ERAWeights returns the input `data.table` with additional rows appended for derived outcomes.
#' @export
DeriveOutcomes<-function(Data,
                         RmPartial,
                         DoBCR_VC){

# Remove partial practices?
Data<-if(RmPartial==T){
  Data[Partial.Outcome.Name==""]
}else{
  Data
}

# If function returns NULL then no derived data are present

# Gross Return (120) - Total Cost (150) = Net Return (124)

Returns.Fun<-function(Data,ACode=120,BCode=150,CCode=124){

  OutcomeCodes<-data.table(ERAg::OutcomeCodes)

  X<-Data
  # Add ID field
  X[,IDx:=paste(TID,CID,EU,SubPrName,SubPrName.Base,Code,M.Year,Site.ID,Variety,Tree,Duration,EU,Units)]

  # Remove groups that already have BCR outcome present
  X<-X[,Out.Present:=any(Outcode %in% CCode),by=IDx][Out.Present!=T]

  X<-X[Outcode %in% c(ACode,BCode)][,BothPresent:=length(unique(Outcode))==2,by=IDx]
  C<-X[BothPresent==T]
  if(nrow(X)==0){
    NULL
  }else{
    X[,N:=.N,by=IDx]

    # There should only be 2 practices, differences could be due to data being presented in two different places
    # In any case it probably indicates an error that should be investigated
    More.Than.2.SubPrNames<-rbind(
      X[N>2,list(T.Descrip,TID,C.Descrip,CID,Code,M.Year,MeanT,MeanC,Outcode,DataLoc,EU),IDx][order(IDx),!"IDx"][Outcode==ACode],
      X[N>2,list(T.Descrip,TID,C.Descrip,CID,Code,M.Year,MeanT,MeanC,Outcode,DataLoc,EU),IDx][order(IDx),!"IDx"][Outcode==BCode]
    )

    # Once checking data for those papers with >2 add the dataloc to the IDx field
    X[N>2,IDx:=paste(IDx,DataLoc)]

    # Check cost and return are still present
    X<-X[,BothPresent:=length(unique(Outcode))==2,by=IDx]
    X<-X[BothPresent==T]

    X[,N:=.N,by=IDx]

    # Has adding dataloc "resolved" issue?
    Error1.Unresolved.by.Dataloc<-rbind(
      X[N!=2,list(T.Descrip,C.Descrip,Code,M.Year,MeanT,MeanC,Outcode,DataLoc),IDx][order(IDx),!"IDx"][Outcode==ACode],
      X[N!=2,list(T.Descrip,C.Descrip,Code,M.Year,MeanT,MeanC,Outcode,DataLoc),IDx][order(IDx),!"IDx"][Outcode==BCode]
    )

    # Remove problem observations
    X<-X[N==2]

    Y<-rbindlist(lapply(X[,unique(IDx)],FUN=function(CODE){
      Z<-X[IDx==CODE,list(Outcode,MeanT,MeanC)]
      Z<-data.table(
        IDx=CODE,
        MeanT=Z[Outcode==ACode,MeanT]-Z[Outcode==BCode,MeanT],
        MeanC=Z[Outcode==ACode,MeanC]-Z[Outcode==BCode,MeanC]
      )
      Z
    }))

    X<-X[Outcode!=BCode]

    X[,Outcode:=CCode
    ][,Out.Pillar:=OutcomeCodes[Code==CCode,Pillar]
    ][,Out.SubPillar:=OutcomeCodes[Code==CCode,Subpillar]
    ][,Out.Ind:=OutcomeCodes[Code==CCode,Indicator]
    ][,Out.SubInd:=OutcomeCodes[Code==CCode,Subindicator]]

    X<-X[,!c("MeanC","MeanT")]

    X<-cbind(X,Y[match(IDx,X[,IDx])])
    X<-X[,!c("IDx","N","Out.Present","BothPresent")]

    return(list(data=X,errors1=More.Than.2.SubPrNames,errors2=Error1.Unresolved.by.Dataloc))
  }

}

NR<-Returns.Fun(Data=data.table::copy(Data),ACode=120,BCode=150,CCode=124)
GM<-Returns.Fun(Data=data.table::copy(Data),ACode=120,BCode=152,CCode=124.1)


KeepCols<-colnames(NR$data)[colnames(NR$data) %in% colnames(Data)]
NR$data<-NR$data[,..KeepCols]
GM$data<-GM$data[,..KeepCols]

nrow(NR$data)/nrow(Data[Outcode==124])
nrow(NR$data)/nrow(Data[Outcode==124.1])

# Combine with master dataset ####
Data<-rbindlist(list(Data,NR$data),use.names=T)

# Choose Outcome Codes
# Net returns to total/variable costs
CodesNet<-c(125.1,125.3,126.1,126.3)

# Gross returns to total/variable costs
CodesGross<-c(125.0,125.2,126.0,126.2)

# If function returns NULL then no derived data are present
BCR.Fun<-function(Data,CostCode,ReturnCode,RatioCode){

  OutcomeCodes<-data.table(ERAg::OutcomeCodes)

  X<-Data
  # Add ID field
  X[,IDx:=paste(TID,CID,EU,SubPrName,SubPrName.Base,Code,M.Year,Site.ID,Variety,Tree,Duration,EU,Units)]

  # Remove groups that already have BCR outcome present
  X<-X[,BCR.Present:=any(Outcode %in% RatioCode),by=IDx][BCR.Present!=T]

  X<-X[Outcode %in% c(ReturnCode,CostCode)][,BothPresent:=length(unique(Outcode))==2,by=IDx]
  X<-X[BothPresent==T]
  if(nrow(X)==0){
    NULL
  }else{
    X[,N:=.N,by=IDx]

    # There should only be 2 practices, differences could be due to data being presented in two different places
    # In any case it probably indicates an error that should be investigated
    More.Than.2.SubPrNames<-rbind(
      X[N>2,list(T.Descrip,TID,C.Descrip,CID,Code,M.Year,MeanT,MeanC,Outcode,DataLoc,EU),IDx][order(IDx),!"IDx"][Outcode==CostCode],
      X[N>2,list(T.Descrip,TID,C.Descrip,CID,Code,M.Year,MeanT,MeanC,Outcode,DataLoc,EU),IDx][order(IDx),!"IDx"][Outcode==ReturnCode]
    )

    # Once checking data for those papers with >2 add the dataloc to the IDx field
    X[N>2,IDx:=paste(IDx,DataLoc)]

    # Check cost and return are still present
    X<-X[,BothPresent:=length(unique(Outcode))==2,by=IDx]
    X<-X[BothPresent==T]

    X[,N:=.N,by=IDx]

    # Has adding dataloc "resolved" issue?
    Error1.Unresolved.by.Dataloc<-rbind(
      X[N!=2,list(T.Descrip,C.Descrip,Code,M.Year,MeanT,MeanC,Outcode,DataLoc),IDx][order(IDx),!"IDx"][Outcode==CostCode],
      X[N!=2,list(T.Descrip,C.Descrip,Code,M.Year,MeanT,MeanC,Outcode,DataLoc),IDx][order(IDx),!"IDx"][Outcode==ReturnCode]
    )

    # Remove problem observations
    X<-X[N==2]

    Y<-rbindlist(lapply(X[,unique(IDx)],FUN=function(CODE){
      Z<-X[IDx==CODE,list(Outcode,MeanT,MeanC)]
      Z<-data.table(
        IDx=CODE,
        MeanT=Z[Outcode==ReturnCode,MeanT]/Z[Outcode==CostCode,MeanT],
        MeanC=Z[Outcode==ReturnCode,MeanC]/Z[Outcode==CostCode,MeanC]
      )
      Z
    }))

    X<-X[Outcode!=ReturnCode]

    X[,Outcode:=RatioCode
    ][,Out.Pillar:=OutcomeCodes[Code==RatioCode,Pillar]
    ][,Out.SubPillar:=OutcomeCodes[Code==RatioCode,Subpillar]
    ][,Out.Ind:=OutcomeCodes[Code==RatioCode,Indicator]
    ][,Out.SubInd:=OutcomeCodes[Code==RatioCode,Subindicator]]

    X<-X[,!c("MeanC","MeanT")]

    X<-cbind(X,Y[match(IDx,X[,IDx])])
    X<-X[,!c("IDx","N","BCR.Present","BothPresent")]

    return(list(data=X,errors1=More.Than.2.SubPrNames,errors2=Error1.Unresolved.by.Dataloc))
  }

}

# Gross Return (120) / Total Cost (150) = Benefit Cost Ratio (125) ####
GRTC<-BCR.Fun(Data=data.table::copy(Data),CostCode=150,ReturnCode=120,RatioCode=125)
# Net Return (124) / Total Cost (150) = Benefit Cost Ratio (125.1) ####
NRTC<-BCR.Fun(Data=data.table::copy(Data),CostCode=150,ReturnCode=124,RatioCode=125.1)

KeepCols<-colnames(GRTC$data)[colnames(GRTC$data) %in% colnames(Data)]
GRTC$data<-GRTC$data[,..KeepCols]
NRTC$data<-NRTC$data[,..KeepCols]

if(DoBCR_VC==T){
  # Gross Return (120) / Variable Cost (152) = Benefit Cost Ratio (125.2) ####
  GRVC<-BCR.Fun(Data=data.table::copy(Data),CostCode=152,ReturnCode=120,RatioCode=125.2)
  # Gross Return (124) / Variable Cost (152) = Benefit Cost Ratio (125.3) ####
  NRVC<-BCR.Fun(Data=data.table::copy(Data),CostCode=152,ReturnCode=124,RatioCode=125.3)

  NRVC$data<-NRVC$data[,..KeepCols]
  GRVC$data<-GRVC$data[,..KeepCols]

  # Combine with master dataset ####
  Data<-rbindlist(list(Data,GRTC$data,NRTC$data,NRVC$data,GRVC$data),use.names=T)

}else{
  # Combine with master dataset ####
  Data<-rbindlist(list(Data,GRTC$data,NRTC$data),use.names=T)
}

# Remove any infinite values caused by MeanC being 0
Data<-Data[!(is.infinite(MeanC)|is.infinite(MeanT))]
return(Data)

}
