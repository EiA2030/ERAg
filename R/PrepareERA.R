#' Prepare ERA Data
#'
#' This function prepares the ERA dataset for the analysis of outcome ratios. Several actions are applied:
#' 1) Where a lower value indicates a better outcome, the outcome values for control (*MeanC*) and treatment (*MeanT*) are swapped. This excludes economic outcomes.
#' A logical column *MeanFlip* is added to the output dataset to indicate if outcome values have been swapped. Response ratios (*yi*) and percentage change (*pc*) are recalculated.
#' 2) Non-ERA practices (h-codes) are removed from the base.practice list (eventually this will be moved to compilation).
#' 3) Rows with blank practice names (*PrName*) are filtered from the supplied dataset.
#' 4) The inverse of efficiency outcomes that are In/Out (267.1 and 265.1) is taken (1/value) and the outcome code (267 & 265) and naming columns are renamed accordingly.
#' 5) Outcomes with a percentage of negative values > **Perc.Neg** are filtered from the supplied dataset.
#' 6) Practice names (*PrNames*) can optionally be be encoded and output as combinations. For more details see the **DoCombinations** and **CombineAll** parameter descriptions.
#' @param Data The compiled ERA dataset
#' @param DoCombinations Logical `T/F`. When set to TRUE rows with >1 experimental practice are duplicated for the number of practices present and PrName/SubPrName fields changed to one of those practices
#' (e.g. if PrName is Mulch-BioChar then the row is duplicated (n=2) with PrName in row 1 as Mulch and in row 2 as BioChar). The copied rows are joined and the function output changes to a list of two data.tables, "Data"
#' and "Data.Combos" . Note ONLY the PrName/SubPrName field is changed in the Data.Combos table, all other fields remain the same. Default = `F`.
#' @param CombineAll Logical `T/F`. Applies only when DoCombinations is `TRUE.` When set to TRUE all rows containing a practice (alone or in combination with other practices) are copied to the
#' Data.Combos table and renamed. When set FALSE only the rows of a practice where it occurs in combination with other practice are copied to the Data.Combos table and renamed.
#' Default = F.
#' @param Perc.Neg A numeric vector of length one defining the maximum percentage of negative values allowed for an outcome x practice combination. If an outcome x practice has a higher percentage of negative values than `Perc.Neg`
#' it is filtered from the dataset. Default = 0.5.
#' @param Cols A vector of column names to retain from the ERA dataset supplied. Default values are supplied.
#' @param Invert2xNeg Swaps MeanT and MeanC values where they are both negative. Less negative values become better than more negative values.
#' @param PLevel choose `Practice` or `Subpractice` to determine hierarchical level at which practice combinations are detected. To make the function more generalizable you can also include the column name of a field here.
#' @param Delim Character. The delimter that separates multiple names within a character string (for example the delimiter `-` separates two practices in the string `Inorganic Fertilizer-Organic Fertilizer`)
#' @return If DoCombinations = F a data.table of the processed ERA dataset. If DoCombinations = T a list of two data.tables, "Data" as per combinations = F and "Data.Combos" where
#' PrNames/SubPrName have been modified to reflect combination practices.
#' @export
#' @import data.table
#' @importFrom data.table copy setnames rbindlist
PrepareERA<-function(Data,
                    CombineAll=F,
                    DoCombinations=F,
                    Perc.Neg = 0.5,
                    RmNeg = T,
                    Invert2xNeg=T,
                    PLevel="Practice",
                    Delim="-",
                    Cols = c("Code","Country","Latitude","Longitude","Site.Type","ID","Site.ID","Rep","Diversity","Tree","Variety","Duration","M.Year","EU","EUlist",
                             "Outcode","MeanC","MeanC.Error","MeanT","MeanT.Error","Mean.Error.Type","Units","TID","CID","MeanFlip","Neg.Vals","plist","base.list","Product","Product.Type","Product.Subtype",
                             "Product.Simple","Out.Pillar","Out.SubPillar","Out.Ind","Out.SubInd","SubPrName","PrName","Theme","SubPrName.Base","PrName.Base",
                             "T.Descrip", "C.Descrip", "C.NI","C.NO", "T.NI", "T.NO","Partial.Outcome.Name","Partial.Outcome.Code","DataLoc")
){

  DataX<-data.table::copy(Data)

  OutcomeCodes<-data.table::copy(ERAg::OutcomeCodes)
  PracticeCodes<-data.table::copy(ERAg::PracticeCodes)
  EUCodes<-data.table::copy(ERAg::EUCodes)

  if("Negative Values" %in% colnames(OutcomeCodes)){
    setnames(OutcomeCodes,"Negative Values","Negative.Values")
  }


  Flip.Neg<-function(DataX,OutcomeCodes){
    N1<-OutcomeCodes[match(DataX[,Outcode],OutcomeCodes[,Code]),Sign]=="n"
    X<-DataX[N1,c("MeanC","MeanT")]

    DataX[N1,MeanC:=X[,MeanT]][N1,MeanT:=X[,MeanC]]
    DataX[,MeanFlip:="N"][N1,MeanFlip:="Y"]

    return(DataX)
  }

  # Add ID column
  DataX[,ID:=Site.Key]
  #DataX[,Site.Key:=NULL]

  # Remove any instances where PrName is blank
  DataX<-DataX[!PrName==""]

  # Convert 267.1 and 265.1 conversion ratio outcomes (In/Out) into 267 and 265 outcomes (Out/In)
  DataX[,MeanC:=as.numeric(as.character(MeanC))][,MeanT:=as.numeric(as.character(MeanT))]
  DataX[Outcode==267.1,MeanC:=1/MeanC
  ][Outcode==267.1,MeanT:=1/MeanT
  ][Outcode==267.1,Out.SubInd:=OutcomeCodes[Code==267,Subindicator]
  ][Outcode==267.1,Out.SubInd.S:=OutcomeCodes[Code==267,Subindicator.Short]
  ][Outcode==267.1,Out.SubInd.Code:=OutcomeCodes[Code==267,Subindicator.Code]
  ][Outcode==267.1,Outcode:=267]

  DataX[Outcode==265.1,MeanC:=1/MeanC
  ][Outcode==265.1,MeanT:=1/MeanT
  ][Outcode==265.1,Out.SubInd:=OutcomeCodes[Code==265,Subindicator]
  ][Outcode==265.1,Out.SubInd.S:=OutcomeCodes[Code==265,Subindicator.Short]
  ][Outcode==265.1,Out.SubInd.Code:=OutcomeCodes[Code==265,Subindicator.Code]
  ][Outcode==265.1,Outcode:=265]

  # Swap CBR for BCR
  DataX[Outcode==126,MeanC:=1/MeanC
  ][Outcode==126,MeanT:=1/MeanT
  ][Outcode==126,Out.SubInd:=OutcomeCodes[Code==125,Subindicator]
  ][Outcode==126,Out.SubInd.S:=OutcomeCodes[Code==125,Subindicator.Short]
  ][Outcode==126,Out.SubInd.Code:=OutcomeCodes[Code==125,Subindicator.Code]
  ][Outcode==126,Outcode:=125]

  DataX[Outcode==126.1,MeanC:=1/MeanC
  ][Outcode==126.1,MeanT:=1/MeanT
  ][Outcode==126.1,Out.SubInd:=OutcomeCodes[Code==125.1,Subindicator]
  ][Outcode==126.1,Out.SubInd.S:=OutcomeCodes[Code==125.1,Subindicator.Short]
  ][Outcode==126.1,Out.SubInd.Code:=OutcomeCodes[Code==125.1,Subindicator.Code]
  ][Outcode==126.1,Outcode:=125]


  # Filter out outcomes with >Perc.Neg% negative values

  DataX[,Neg.Vals.One:=sum((MeanC<0 & MeanT>0)|(MeanC>0 & MeanT<0),na.rm=T),by=c("Out.SubInd","PrName")
  ][,N.OBs:=.N,by=c("Out.SubInd","PrName")
  ][,Perc.Neg.One:=round(100*Neg.Vals.One/N.OBs,1)
  ][Perc.Neg.One<=Perc.Neg,Neg.Vals:="N"]


  if(Invert2xNeg){
    T.Vals<-DataX[MeanC<0 & MeanT<0,MeanC]
    C.Vals<-DataX[MeanC<0 & MeanT<0,MeanT]
    DataX[MeanC<0 & MeanT<0,MeanC:=T.Vals]
    DataX[MeanC<0 & MeanT<0,MeanT:=C.Vals]
    rm(T.Vals,C.Vals)
  }

  # Remove outcomes where they are negative > Perc.Neg of the time
  if(RmNeg==T){
    DataX<-DataX[!Neg.Vals=="Y"]
  }

  # Remove observations where plist is blank
  DataX<-DataX[plist!=""]

  # Restructure DataX to deal with when negative RR is better
  # Note that this changes the structure of the compendium dataset. The resulting data.table should not be shared as this could lead to confusion.

  DataX<-Flip.Neg(DataX=DataX,OutcomeCodes)

  # Remove unecessary cols
  DataX<-DataX[,..Cols]

  DataX[,pc:=100*((MeanT/MeanC)-1)
  ][,yi:=log(MeanT/MeanC)]

  Target_Field<-PLevel

  if(PLevel=="Practice"){
    Target_Field<-"PrName"
  }

  if(PLevel=="Subpractice"){
    Target_Field<-"SubPrName"
    }

  X<-aggregate_names(Data,
                  CombineAll=CombineAll,
                  DoCombinations=DoCombinations,
                  Target_Field=Target_Field,
                  Delim=Delim)

  return(X)


}
