#' Derive ERA outcomes from existing data
#'
#' This function identifies exactly co-located ERA outcomes (same experiment, treatment, location and time) to
#' generate additional outcomes and appends them to the supplied Data.
#'
#' The function currently generates additional outcomes for:
#' 1) Net Returns = gross returns - total costs
#' 2) Gross Margin = gross returns - variable costs
#' 3) Benefit Cost Ratio (GRTC) =  gross returns/total costs
#' 4) Benefit Cost Ratio (GRVC) =  gross returns/variable costs
#' 5) Water Use Efficiency (WUE) = yield/total seasonal precipitation
#' 6) Nitrogen Total Factor Productivity (NTFP) = (yield.exp - yield.cont)/N added
#'
#' Derived net returns are appended to `Data` before benefit cost ratio outcomes are derived.
#'
#' Note that many of the outcomes derived from the data can be negative therefore be careful not to exclude these outcomes when applying the `ERAg::PrepareERA`
#' function (the `Perc.Neg` parameter could be adjusted to retain negative outcomes).
#'
#' @param Data An ERA data.table (e.g. `ERAg::ERA.Compiled`).
#' @param RmPartial Logical `T/F`. If `T` partial economic outcomes are excluded from the process.
#' @param DoWUE Logical `T/F`. If `T` water use efficiency outcome is derived from yield and TSP (total seasonal precipitation) data
#' @return DeriveOutcomes returns the input `data.table` with additional rows appended for derived outcomes.
#' @export
#' @import data.table
DeriveOutcomes<-function(Data,
                         RmPartial = T,
                         DoWUE = T){

  # Remove partial practices?
  Data<-if(RmPartial==T){
    Data[Partial.Outcome.Name==""]
  }else{
    Data
  }

  # If function returns NULL then no derived data are present


  # Gross Return (120) - Total Cost (150) = Net Return (124)
  NR<-derive_outcome_sub(Data=data.table::copy(Data),target=120,subtract=150,result=124)

  # Gross Return (120) - Variable Cost (152) = Gross Margin (124.1)
  GM<-derive_outcome_sub(Data=data.table::copy(Data),target=120,subtract=152,result=124.1)

  KeepCols<-colnames(NR$data)[colnames(NR$data) %in% colnames(Data)]
  if(!is.null(NR$data)){
    NR$data<-NR$data[,..KeepCols]
  }
  if(!is.null(GM$data)){
    GM$data<-GM$data[,..KeepCols]
  }

  # Combine with master dataset ####
  Data<-rbindlist(list(Data,NR$data),use.names=T)

  # Choose Outcome Codes
  # Net returns to total/variable costs
  CodesNet<-c(125.1,125.3,126.1,126.3)

  # Gross returns to total/variable costs
  CodesGross<-c(125.0,125.2,126.0,126.2)

  # Gross Return (120) / Total Cost (150) = Benefit Cost Ratio (125) ####
  GMTC<-derive_outcome_ratio(Data=data.table::copy(Data),numerator=120,denominator=150,result=125)
  # Gross Return (120) / Variable Cost (152) = Benefit Cost Ratio (125.1) ####
  NRTC<-derive_outcome_ratio(Data=data.table::copy(Data),numerator=120,denominator=152,result=125.1)

  KeepCols<-colnames(GMTC$data)[colnames(GMTC$data) %in% colnames(Data)]

  if(!is.null(GMTC$data)){
    GMTC$data<-GMTC$data[,..KeepCols]
  }

  if(!is.null(NRTC$data)){
    NRTC$data<-NRTC$data[,..KeepCols]
  }

  # Combine with master dataset ####
  Data<-rbindlist(list(Data,GMTC$data,NRTC$data),use.names=T)


  # Add WUE
  if(DoWUE){
    AddWUE<-function(Data,ACode,BCode){

      OutcomeCodes<-data.table(ERAg::OutcomeCodes)

      X<-data.table::copy(Data)
      # Add ID field
      X[,IDx:=paste(TID,CID,EU,SubPrName,SubPrName.Base,Code,M.Year,Site.ID,Diversity,Variety,Tree,Duration,EU,Units)]

      # Remove groups that already have ACode outcome present
      X<-X[,Out.Present:=any(Outcode %in% ACode),by=IDx][Out.Present!=T]
      X[,Out.Present:=NULL]

      X<-X[Outcode == BCode & !is.na(TSP) & !is.na(MeanC)]

      X[,MeanC:=MeanC/TSP
      ][,MeanT:=MeanT/TSP
      ][,Units:=paste0(Units,"/mm"),
      ][,Outcode:=ACode
      ][,IDx:=NULL]

      return(X)
    }
    WUE<-AddWUE(Data=data.table::copy(Data),
                ACode=OutcomeCodes[grep("Water Use Eff",Subindicator),Code],
                BCode=101)

    Data<-rbindlist(list(Data,WUE), use.names = T)
  }

  # Recalculate yi & pc
  Data[,yi:=MeanT/MeanC
  ][,pc:=100*((MeanT-MeanC)/MeanC)-100]

  # Recode outcomes
  OutReturn<-function(OCode,Field){
    return(OutcomeCodes[Code==OCode,..Field])
  }

  Data[,Out.Pillar:=OutReturn(OCode=Outcode[1],Field="Pillar"),by=Outcode]
  Data[,Out.SubPillar:=OutReturn(OCode=Outcode[1],Field="Subpillar"),by=Outcode]
  Data[,Out.Ind:=OutReturn(OCode=Outcode[1],Field="Indicator"),by=Outcode]
  Data[,Out.SubInd:=OutReturn(OCode=Outcode[1],Field="Subindicator"),by=Outcode]

  Data[,Out.Pillar.Code:=OutReturn(OCode=Outcode[1],Field="Pillar.Code"),by=Outcode]
  Data[,Out.SubPillar.Code:=OutReturn(OCode=Outcode[1],Field="Subpillar.Code"),by=Outcode]
  Data[,Out.Ind.Code:=OutReturn(OCode=Outcode[1],Field="Indicator.Code"),by=Outcode]
  Data[,Out.SubInd.Code:=OutReturn(OCode=Outcode[1],Field="Subindicator.Code"),by=Outcode]


  # Remove any infinite values caused by MeanC being 0
  Data<-Data[!(is.infinite(MeanC)|is.infinite(MeanT))]
  return(Data)

}
