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
#' @return If DoCombinations = F a data.table of the processed ERA dataset. If DoCombinations = T a list of two data.tables, "Data" as per combinations = F and "Data.Combos" where
#' PrNames have modified to reflect combination practices.
#' @export
PrepareERA<-function(Data,
                     CombineAll=F,
                     DoCombinations=F,
                     Perc.Neg = 0.5,
                     RmNeg = T,
                     Cols = c("Code","Country","Latitude","Longitude","Site.Type","ID","Site.ID","Rep","Diversity","Tree","Variety","Duration","M.Year","EU","EUlist",
                               "Outcode","MeanC","MeanT","Units","TID","CID","MeanFlip","Neg.Vals","plist","base.list","Product","Product.Type","Product.Subtype",
                               "Product.Simple","Out.Pillar","Out.SubPillar","Out.Ind","Out.SubInd","SubPrName","PrName","Theme","SubPrName.Base","PrName.Base",
                               "T.Descrip", "C.Descrip", "C.NI","C.NO", "T.NI", "T.NO","PrName.Code","SubPrName.Code", "Product.Simple.Code",
                               "Product.Subtype.Code", "Product.Type.Code","Out.Pillar.Code","Out.SubPillar.Code","Out.Ind.Code", "Out.SubInd.Code",
                              "Theme.Code","PrName.Base.Code","SubPrName.Base.Code","Theme.Base.Code","Partial.Outcome.Name","Partial.Outcome.Code")
                     ){
  OutcomeCodes<-data.table(ERAg::OutcomeCodes)
  PracticeCodes<-data.table(ERAg::PracticeCodes)
  EUCodes<-data.table(ERAg::EUCodes)


  Flip.Neg<-function(Data,OutcomeCodes){
    N1<-OutcomeCodes[match(Data[,Outcode],OutcomeCodes[,Code]),Sign]=="n"
    if(!"Out.Ind" %in% colnames(Data)){
      N1[Data[,Outcome=="Costs"]]<-F # Do not flip Costs
    }else{
      N1[Data[,Out.Ind=="Costs"]]<-F # Do not flip Costs
    }
    X<-Data[N1,c("MeanC","MeanT")]

    Data[N1,MeanC:=X[,MeanT]][N1,MeanT:=X[,MeanC]]
    Data[,MeanFlip:="N"][N1,MeanFlip:="Y"]

    return(Data)
  }


  # Remove any h codes from base practice list (**NOTE** should be moved to compilation function)
  HCodes<-PracticeCodes[grep("h",Code),Code]
  Data[,base.list:=unlist(lapply(strsplit(Data$base.list,"-"), FUN=function(H){paste(H[!H %in% HCodes],collapse="-")}))]

  # Add ID column
  Data[,ID:=Site.Key]
  Data[,Site.Key:=NULL]

  # Remove any instances where PrName is blank
  Data<-Data[!PrName==""]

  # Remove Energy from Practice Theme
  # Data<-Data[!grepl("Energy",Theme)]

  # Convert 267.1 and 265.1 conversion ratio outcomes (In/Out) into 267 and 265 outcomes (Out/In)
  Data[,MeanC:=as.numeric(as.character(MeanC))][,MeanT:=as.numeric(as.character(MeanT))]
  Data[Outcode==267.1,MeanC:=1/MeanC
       ][Outcode==267.1,MeanT:=1/MeanT
         ][Outcode==267.1,Out.SubInd:=OutcomeCodes[Code==267,Subindicator]
           ][Outcode==267.1,Out.SubInd.S:=OutcomeCodes[Code==267,Subindicator.Short]
              ][Outcode==267.1,Out.SubInd.Code:=OutcomeCodes[Code==267,Subindicator.Code]
                 ][Outcode==267.1,Outcode:=267]


  Data[Outcode==265.1,MeanC:=1/MeanC
       ][Outcode==265.1,MeanT:=1/MeanT
        ][Outcode==265.1,Out.SubInd:=OutcomeCodes[Code==265,Subindicator]
           ][Outcode==265.1,Out.SubInd.S:=OutcomeCodes[Code==265,Subindicator.Short]
            ][Outcode==265.1,Out.SubInd.Code:=OutcomeCodes[Code==265,Subindicator.Code]
             ][Outcode==265.1,Outcode:=265]


  # Filter out outcomes with >Perc.Neg% negative values

  Data[,Neg.Vals:=OutcomeCodes[match(Data[,Outcode],Code),Negative.Values]]

  A<-Data[Neg.Vals=="Y"
    ][,list(Neg.Vals.MeanC=sum(MeanC<0,na.rm=T),
            Neg.Vals.MeanT=sum(MeanT<0,na.rm=T),
            Neg.Vals.Both=sum(MeanC<0 & MeanT<0,na.rm=T),
            Neg.Vals.Any=sum(MeanC<0 | MeanT<0,na.rm=T),
            N.OBs=.N),by=c("Outcode")
      ][,Perc.Neg.Any:=round(100*Neg.Vals.Any/N.OBs,1)
        ][,Perc.Neg.One:=round(100*(Neg.Vals.Any-Neg.Vals.Both)/N.OBs,1)
         ][,Outname:=OutcomeCodes$Subindicator[match(Outcode,OutcomeCodes$Code)]
          ][order(N.OBs,decreasing = T)]

  Data[Outcode %in% A[Perc.Neg.Any<=Perc.Neg,Outcode],Neg.Vals:="N"]


  OutcomeCodes$Negative.Values[OutcomeCodes$Code %in% A[Perc.Neg.Any<=Perc.Neg,Outcode]]<-"N"

  # Remove outcomes where they are negative > Perc.Neg of the time
    Data<-Data[!Neg.Vals=="Y"]


  # Recode Neg.Vals in FCR/PCR to be N (these are dealt with using a different system to RRs)
  # Data[Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"),Neg.Vals:="N"]

  # Remove observations where plist is blank
  Data<-Data[plist!=""]

  # Restructure Data to deal with when negative RR is better
  # Note that this changes the structure of the compendium dataset. The resulting data.table should not be shared as this could lead to confusion.

  Data<-Flip.Neg(Data=Data,OutcomeCodes)

  # Remove unecessary cols
  Data<-Data[,..Cols]

  Practices<-unique(unlist(strsplit(unique(Data$PrName),"-")))

  if(DoCombinations){
    if(CombineAll){

      Combinations<-rbindlist(lapply(Practices,FUN=function(X){
        N<-grep(X,Data$PrName)
        if(length(N)>0){
          Data[N,][,PrName:=paste0(X," Combinations")]
        }else{

        }
      }))
    }else{

      Combinations<-rbindlist(lapply(Practices,FUN=function(X){
        N<-grep(X,Data$PrName)
        N1<-N[grep("-",Data$PrName[N])]
        if(length(N)>0){
          Data[N1,][,PrName:=paste0(X," Combinations")]
        }else{

        }
      }))
    }

    Data<-rbind(Data,Combinations)

  }

  # Code Practice & Subpractice classes
  # 1) "Solo TPrac No Base Prac" = (A in treatment - nothing in control)  = treatment of A
  # 2) "Multi TPrac No Base Prac" = (AB in treatment - nothing in control) = treatment of AB
  # 3) "Solo TPrac + Base Prac" = (ABC in treatment - BC control) = subtracted treatment of A
  # 4) "Multi TPrac + Base Prac" = (ABC in treatment - C control) = subtracted treatment of AB

  Data[,Pr.Class:=as.character(NA)][,SubPr.Class:=as.character(NA)]
  Data[,Pr.Class.A:=as.character(NA)][,SubPr.Class.A:=as.character(NA)]
  Data[,Pr.Class.B:=as.character(NA)][,SubPr.Class.B:=as.character(NA)]

  # Practices
  Data[!grepl("-",PrName) & PrName.Base=="" & !grepl("Combination",PrName),Pr.Class:="Solo TPrac No Base Prac"]
  Data[grepl("-",PrName) & PrName.Base=="" & !grepl("Combination",PrName),Pr.Class:="Multi TPrac No Base Prac"]
  Data[!grepl("-",PrName) & PrName.Base!="" & !grepl("Combination",PrName),Pr.Class:="Solo TPrac + Base Prac"]
  Data[grepl("-",PrName) & PrName.Base!="" & !grepl("Combination",PrName),Pr.Class:="Multi TPrac + Base Prac"]

  Data[PrName.Base=="" & !grepl("Combination",PrName),Pr.Class.A:="Solo"]
  Data[PrName.Base!="" & !grepl("Combination",PrName),Pr.Class.A:="Multi"]
  Data[!grepl("-",PrName) & !grepl("Combination",PrName),Pr.Class.B:="No Base"]
  Data[grepl("-",PrName) & !grepl("Combination",PrName),Pr.Class.B:="Base"]

  # SubPractices
  Data[!grepl("-",SubPrName) & SubPrName.Base=="" & !grepl("Combination",PrName),SubPr.Class:="Solo TPrac No Base Prac"]
  Data[grepl("-",SubPrName) & SubPrName.Base=="" & !grepl("Combination",PrName),SubPr.Class:="Multi TPrac No Base Prac"]
  Data[!grepl("-",SubPrName) & SubPrName.Base!="" & !grepl("Combination",PrName),SubPr.Class:="Solo TPrac + Base Prac"]
  Data[grepl("-",SubPrName) & SubPrName.Base!="" & !grepl("Combination",PrName),SubPr.Class:="Multi TPrac + Base Prac"]

  Data[SubPrName.Base=="" & !grepl("Combination",PrName),SubPr.Class.A:="Solo"]
  Data[SubPrName.Base!="" & !grepl("Combination",PrName),SubPr.Class.A:="Multi"]
  Data[!grepl("-",SubPrName) & !grepl("Combination",PrName),SubPr.Class.B:="No Base"]
  Data[grepl("-",SubPrName) & !grepl("Combination",PrName),SubPr.Class.B:="Base"]

  # Calculate response ratios &  exclude indicators that can have a negative value
  Data<-suppressWarnings(Data[Neg.Vals=="N"
             ][,pc:=100*((MeanT/MeanC)-1)
               ][,yi:=log(MeanT/MeanC)
                 ][,Neg.Vals:=NULL])

  # This code previously
  #[!Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"),yi:=log(MeanT/MeanC) # Calculate RR
  #][!((is.infinite(yi)|is.na(yi)) & !Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)")) # Remove RRs with NA or infinite values
    # Create a difference for use with FCR and PCR
  #][Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"),yi:=MeanT-MeanC
  #][Out.SubInd %in% c("Feed Conversion Ratio (FCR)","Protein Conversion Ratio (PCR)"),Out.SubPillar:="Conversion Efficiency"


  # Split and duplicate products - disabled
  if(F){
    Data<-rbindlist(lapply(Data[,unique(Product)],FUN=function(X){
      Y<-Data[Product==X]
      Z<-unlist(strsplit(unique(Y$Product)," x "))
      rbindlist(lapply(Z,FUN=function(PROD){
        Y[,Product:=PROD]
      }))
    }))

    # Update Product Type & Sub Type
    Data[, Product.Type:=EUCodes$Product.Type[match(Data$Product,EUCodes$Product)]]
    Data[, Product.Subtype:=EUCodes$Product.Subtype[match(Data$Product,EUCodes$Product)]]
  }

  # Split out combinations data
  if(DoCombinations){
    # Subset and rbind solo and combination practices (as labelled in PrName) into a new data.table
    Data.Combos<-rbind(Data[grepl("Combination",PrName)],Data[Pr.Class.A == "Solo"])
    Data.Combos<-Data.Combos[!grepl("-",PrName)]
    # Create a combination column describing if the (subtracted) practice is in combination or alone
    Data.Combos[,Is.Combo:="Combinations"]
    Data.Combos[Pr.Class.A=="Solo",Is.Combo:="Solo"]
    # Remove uncessary columns
    Data.Combos<-Data.Combos[,!c("Pr.Class","SubPr.Class","Pr.Class.A","SubPr.Class.A","SubPr.Class.B","Pr.Class.B")]
    # Remove combinations from original data.table
    Data<-Data[!grepl("Combinations",PrName)]
    # Remove "Combinations" from PrName in new data.table
    Data.Combos[,PrName:=gsub(" Combinations","",PrName)]
    # Update theme
    Data.Combos[,Theme:=PracticeCodes$Theme[match(Data.Combos$PrName,PracticeCodes$Practice)]]

    return(list(Data=Data,Data.Combos=Data.Combos))

  }else{
    return(Data)
  }

}
