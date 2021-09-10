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
#' 7) Practice (*Pr.Class*) and Subpractice (*SubPr.Class*) class columns are appended to the supplied dataset. Note these are no applied to the Data.Combos output.
#'Values in these columns are as follows:
#'    - *Solo TPrac No Base Prac* = (A in experimental - nothing in control)  = set difference of A + no shared ERA base practice.
#'    - *Multi TPrac No Base Prac* = (AB in experimental - nothing in control) = set difference of AB + no shared ERA base practice.
#'    - *Solo TPrac + Base Prac* = (ABC in experimental - BC control) = set difference of A + shared ERA base practice of BC.
#'    - *Multi TPrac + Base Prac* = (ABC in experimental - C control) = set difference of AB  + shared ERA base practice of C.
#' 8) The columns in 7) are disaggregated into two columns each:
#'    - Class.A (e.g. colname = *Pr.Class.A*) which indicates the presence of single (value = *Solo*) or multiple (value = *Multi*) experimental ERA practices.
#'    - ClassB which indicates the presence (value = *Base*) or absence (value = *No Base*) of shared ERA practices between the control and experimental treatments.
#' 9) *DISABLED: Where multiple products are present in a row the row is duplicated for the length of the products, each row being assigned to one of the products.*
#'
#' @param Data The compiled ERA dataset
#' @param DoCombinations Logical `T/F`. When set to TRUE for each individual ERA practice in the practice codes file the function copies rows where a practice is present
#' and in combination with other practices (no. practices >1) in the in the set difference practice codes (plist column). In the copied rows the PrName is changed by pasting
#' the focal practice (e.g. "A") with  "Combinations" (giving "A Combinations"). The copied rows are joined and the function output changes to a list of two data.tables, "Data"
#' and "Data.Combos" the latter containing data encoded as combinations. Note ONLY the PrName field is changed in the Data.Combos table, all other fields remain the same. This
#' functionality is not implemented for subpractices. Default = `F`.
#' @param CombineAll Logical `T/F`. Applies only when DoCombinations is `TRUE.` When set to TRUE all rows containing a practice (alone or in combination with other practices) are copied to the
#' Data.Combos table and renamed. When set FALSE only the rows of a practice where it occurs in combination with other practice are copied to the Data.Combos table and renamed.
#' Default = F.
#' @param Perc.Neg A numeric vector of length one defining the maximum percentage of negative values allowed for an outcome. If an outcome has more negative values than the number specified
#' it is filtered from the dataset. Default = 0.5.
#' @param Cols A vector of column names to retain from the ERA dataset supplied. Default values are supplied.
#' @param Invert2xNeg Swaps MeanT and MeanC values where they are both negative. Less negative values become better than more negative values.
#' @return If DoCombinations = F a data.table of the processed ERA dataset. If DoCombinations = T a list of two data.tables, "Data" as per combinations = F and "Data.Combos" where
#' PrNames have modified to reflect combination practices.
#' @export
PrepareERA<-function(Data,
                     CombineAll=F,
                     DoCombinations=F,
                     Perc.Neg = 0.5,
                     RmNeg = T,
                     Invert2xNeg=T,
                     Cols = c("Code","Country","Latitude","Longitude","Site.Type","ID","Site.ID","Rep","Diversity","Tree","Variety","Duration","M.Year","EU","EUlist",
                               "Outcode","MeanC","MeanT","Units","TID","CID","MeanFlip","Neg.Vals","plist","base.list","Product","Product.Type","Product.Subtype",
                               "Product.Simple","Out.Pillar","Out.SubPillar","Out.Ind","Out.SubInd","SubPrName","PrName","Theme","SubPrName.Base","PrName.Base",
                               "T.Descrip", "C.Descrip", "C.NI","C.NO", "T.NI", "T.NO","PrName.Code","SubPrName.Code", "Product.Simple.Code",
                               "Product.Subtype.Code", "Product.Type.Code","Out.Pillar.Code","Out.SubPillar.Code","Out.Ind.Code", "Out.SubInd.Code",
                              "Theme.Code","PrName.Base.Code","SubPrName.Base.Code","Theme.Base.Code","Partial.Outcome.Name","Partial.Outcome.Code")
                     ){
  DataX<-data.table::copy(Data)

  OutcomeCodes<-data.table(ERAg::OutcomeCodes)
  PracticeCodes<-data.table(ERAg::PracticeCodes)
  EUCodes<-data.table(ERAg::EUCodes)

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

  # Remove any h codes from base practice list (**NOTE** should be moved to compilation function)
  HCodes<-PracticeCodes[grep("h",Code),Code]
  DataX[,base.list:=unlist(lapply(strsplit(DataX$base.list,"-"), FUN=function(H){paste(H[!H %in% HCodes],collapse="-")}))]

  # Add ID column
  DataX[,ID:=Site.Key]
  DataX[,Site.Key:=NULL]

  # Remove any instances where PrName is blank
  DataX<-DataX[!PrName==""]

  # Remove Energy from Practice Theme
  # DataX<-DataX[!grepl("Energy",Theme)]

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


  # Filter out outcomes with >Perc.Neg% negative values

  DataX[,Neg.Vals:=OutcomeCodes[match(DataX[,Outcode],Code),Negative.Values]]

  A<-DataX[Neg.Vals=="Y"
    ][,list(Neg.Vals.MeanC=sum(MeanC<0,na.rm=T),
            Neg.Vals.MeanT=sum(MeanT<0,na.rm=T),
            Neg.Vals.Both=sum(MeanC<0 & MeanT<0,na.rm=T),
            Neg.Vals.Any=sum((MeanC<0 | MeanT<0) & !(MeanC<0 & MeanT<0),na.rm=T),
            N.OBs=.N),by=c("Outcode")
      ][,Perc.Neg.Any:=round(100*Neg.Vals.Any/N.OBs,1)
        ][,Perc.Neg.One:=round(100*(Neg.Vals.Any-Neg.Vals.Both)/N.OBs,1)
         ][,Outname:=OutcomeCodes$Subindicator[match(Outcode,OutcomeCodes$Code)]
          ][order(N.OBs,decreasing = T)]

  DataX[Outcode %in% A[Perc.Neg.One<=Perc.Neg,Outcode],Neg.Vals:="N"]


  OutcomeCodes$Negative.Values[OutcomeCodes$Code %in% A[Perc.Neg.One<=Perc.Neg,Outcode]]<-"N"

  if(Invert2xNeg){
    T.Vals<-DataX[MeanC<0 & MeanT<0,MeanC]
    C.Vals<-DataX[MeanC<0 & MeanT<0,MeanT]
    DataX[MeanC<0 & MeanT<0,MeanC:=T.Vals]
    DataX[MeanC<0 & MeanT<0,MeanT:=C.Vals]
    rm(T.Vals,C.Vals)
  }

  # Remove outcomes where they are negative > Perc.Neg of the time
  if(RmNeg){
    DataX<-DataX[!Neg.Vals=="Y"]
  }

  # Recode Neg.Vals in FCR/PCR to be N (these are dealt with using a different system to RRs)
  # DataX[Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"),Neg.Vals:="N"]

  # Remove observations where plist is blank
  DataX<-DataX[plist!=""]

  # Restructure DataX to deal with when negative RR is better
  # Note that this changes the structure of the compendium dataset. The resulting data.table should not be shared as this could lead to confusion.

  DataX<-Flip.Neg(DataX=DataX,OutcomeCodes)

  # Remove unecessary cols
  DataX<-DataX[,..Cols]

  Practices<-unique(unlist(strsplit(unique(DataX$PrName),"-")))

  if(DoCombinations){
    if(CombineAll){

      Combinations<-rbindlist(lapply(Practices,FUN=function(X){
        N<-grep(X,DataX$PrName)
        if(length(N)>0){
          DataX[N,][,PrName:=paste0(X," Combinations")]
        }else{

        }
      }))
    }else{

      Combinations<-rbindlist(lapply(Practices,FUN=function(X){
        N<-grep(X,DataX$PrName)
        N1<-N[grep("-",DataX$PrName[N])]
        if(length(N)>0){
          DataX[N1,][,PrName:=paste0(X," Combinations")]
        }else{

        }
      }))
    }

    DataX<-rbind(DataX,Combinations)

  }

  # Code Practice & Subpractice classes
  # 1) "Solo TPrac No Base Prac" = (A in treatment - nothing in control)  = treatment of A
  # 2) "Multi TPrac No Base Prac" = (AB in treatment - nothing in control) = treatment of AB
  # 3) "Solo TPrac + Base Prac" = (ABC in treatment - BC control) = subtracted treatment of A
  # 4) "Multi TPrac + Base Prac" = (ABC in treatment - C control) = subtracted treatment of AB

  DataX[,Pr.Class:=as.character(NA)][,SubPr.Class:=as.character(NA)]
  DataX[,Pr.Class.A:=as.character(NA)][,SubPr.Class.A:=as.character(NA)]
  DataX[,Pr.Class.B:=as.character(NA)][,SubPr.Class.B:=as.character(NA)]

  # Practices
  DataX[!grepl("-",PrName) & PrName.Base=="" & !grepl("Combination",PrName),Pr.Class:="Solo TPrac No Base Prac"]
  DataX[grepl("-",PrName) & PrName.Base=="" & !grepl("Combination",PrName),Pr.Class:="Multi TPrac No Base Prac"]
  DataX[!grepl("-",PrName) & PrName.Base!="" & !grepl("Combination",PrName),Pr.Class:="Solo TPrac + Base Prac"]
  DataX[grepl("-",PrName) & PrName.Base!="" & !grepl("Combination",PrName),Pr.Class:="Multi TPrac + Base Prac"]

  DataX[PrName.Base=="" & !grepl("Combination",PrName),Pr.Class.A:="Solo"]
  DataX[PrName.Base!="" & !grepl("Combination",PrName),Pr.Class.A:="Multi"]
  DataX[!grepl("-",PrName) & !grepl("Combination",PrName),Pr.Class.B:="No Base"]
  DataX[grepl("-",PrName) & !grepl("Combination",PrName),Pr.Class.B:="Base"]

  # SubPractices
  DataX[!grepl("-",SubPrName) & SubPrName.Base=="" & !grepl("Combination",PrName),SubPr.Class:="Solo TPrac No Base Prac"]
  DataX[grepl("-",SubPrName) & SubPrName.Base=="" & !grepl("Combination",PrName),SubPr.Class:="Multi TPrac No Base Prac"]
  DataX[!grepl("-",SubPrName) & SubPrName.Base!="" & !grepl("Combination",PrName),SubPr.Class:="Solo TPrac + Base Prac"]
  DataX[grepl("-",SubPrName) & SubPrName.Base!="" & !grepl("Combination",PrName),SubPr.Class:="Multi TPrac + Base Prac"]

  DataX[SubPrName.Base=="" & !grepl("Combination",PrName),SubPr.Class.A:="Solo"]
  DataX[SubPrName.Base!="" & !grepl("Combination",PrName),SubPr.Class.A:="Multi"]
  DataX[!grepl("-",SubPrName) & !grepl("Combination",PrName),SubPr.Class.B:="No Base"]
  DataX[grepl("-",SubPrName) & !grepl("Combination",PrName),SubPr.Class.B:="Base"]

  # Calculate response ratios &  exclude indicators that can have a negative value
  DataX<-suppressWarnings(DataX[Neg.Vals=="N"
             ][,pc:=100*((MeanT/MeanC)-1)
               ][,yi:=log(MeanT/MeanC)
                 ][,Neg.Vals:=NULL])

    # Split and duplicate products - disabled
  if(F){
    DataX<-rbindlist(lapply(DataX[,unique(Product)],FUN=function(X){
      Y<-DataX[Product==X]
      Z<-unlist(strsplit(unique(Y$Product)," x "))
      rbindlist(lapply(Z,FUN=function(PROD){
        Y[,Product:=PROD]
      }))
    }))

    # Update Product Type & Sub Type
    DataX[, Product.Type:=EUCodes$Product.Type[match(DataX$Product,EUCodes$Product)]]
    DataX[, Product.Subtype:=EUCodes$Product.Subtype[match(DataX$Product,EUCodes$Product)]]
  }

  # Split out combinations data
  if(DoCombinations){
    # Subset and rbind solo and combination practices (as labelled in PrName) into a new data.table
    Data.Combos<-rbind(DataX[grepl("Combination",PrName)],DataX[Pr.Class.A == "Solo"])
    Data.Combos<-Data.Combos[!grepl("-",PrName)]
    # Create a combination column describing if the (subtracted) practice is in combination or alone
    Data.Combos[,Is.Combo:="Combinations"]
    Data.Combos[Pr.Class.A=="Solo",Is.Combo:="Solo"]
    # Remove uncessary columns
    Data.Combos<-Data.Combos[,!c("Pr.Class","SubPr.Class","Pr.Class.A","SubPr.Class.A","SubPr.Class.B","Pr.Class.B")]
    # Remove combinations from original data.table
    DataX<-DataX[!grepl("Combinations",PrName)]
    # Remove "Combinations" from PrName in new data.table
    Data.Combos[,PrName:=gsub(" Combinations","",PrName)]
    # Update theme
    Data.Combos[,Theme:=PracticeCodes$Theme[match(Data.Combos$PrName,PracticeCodes$Practice)]]

    return(list(Data=DataX,Data.Combos=Data.Combos))

  }else{
    return(DataX)
  }

}
