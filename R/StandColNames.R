#' Rename Concept Columns
#'
#' `StandColNames` renames ERA columns as per user selected values for organizational levels of Practice, Outcome and Product (EU) concepts.
#'
#' The columns in `Data` corresponding to the organizational levels selected are renamed to `Practice`, `Outcome` and `Product`. This standardization of
#' column naming allows generalization of column naming for use with other functions in the `ERA` package  that assume these columns are present. This is
#' particularly useful for the creation of interactive apps where the user can choose to explore the data at different levels within the ERA concept hierarchy.
#'
#' See the `CreateConceptLevels` function how parameter codes are translated to ERA column names.
#'
#' @param Data a `data.frame` or `data.table` of ERA data (e.g. `ERA.Compiled`).
#' @param PLevel character vector of length one; accepted values:  `T` theme, `P` practice or `S` subpractice. This parameter indicates the **practice**
#' level for analysis. Set to argument to `NA` to leave `Data` practice column names unchanged.
#' @param OLevel character vector of length one; accepted values:  `P` pillar, `SP` subpillar, `I` indicator or `SI` subindicator. This parameter
#' indicates the **outcome** level for analysis. Set to argument to `NA` to leave `Data` outcome column names unchanged.
#' @param EULevel character vector of length one; accepted values:  `T` type, `S` subtype  or `P` product. This parameter indicates the **product**
#' level for analysis. *Note products are also referred to as experimental units (EU) in ERA*. Set to argument to `NA` to leave `Data` product column names unchanged.
#' @return `StandColNames` returns a `data.table` of the supplied dataset with focal columns renamed to `Practice`, `Practice.Code`, `Practice.Base`,
#' `Practice.Base.Code`, `Outcome`, `Outcome.Code`, `Product`, and `Product Code`.
#' @export
StandColNames<-function(Data,
                        PLevel=NA,
                        OLevel=NA,
                        EULevel=NA){
  Data<-data.table(Data)

  if(!is.na(PLevel)){
    Prac<-ERAConcepts$Prac.Levels
    Prac<-Prac$Prac[Prac$Choice.Code==PLevel]
    setnames(Data,Prac,"Practice")
    setnames(Data,paste0(Prac,".Code"),"Practice.Code")
    setnames(Data,paste0(Prac,".Base"),"Practice.Base")
    setnames(Data,paste0(Prac,".Base.Code"),"Practice.Base.Code")
  }

  if(!is.na(OLevel)){
    Out<-ERAConcepts$Out.Levels
    Out<-Out$Out[Out$Choice.Code==OLevel]
    setnames(Data,Out,"Outcome")
    setnames(Data,paste0(Out,".Code"),"Outcome.Code")
  }

  if(!is.na(EULevel)){
    Prod<-ERAConcepts$Prod.Levels
    Prod<-Prod$Prod[Prod$Choice.Code==EULevel]
    setnames(Data,Prod,"Product")
    setnames(Data,paste0(Prod,".Code"),"Product.Code")
  }

  return(Data)

}
