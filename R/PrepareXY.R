#' Prepare ERA Data for XY Plot
#'
#' `PrepareXY` standardizes ERA column names for use in generalized plotting functions that require selection of two out of three of the higher order
#' ERA concepts of  **practice**, **outcome** and **product**.
#'
#' @param Data a `data.frame` or `data.table` of ERA data (e.g. `ERA.Compiled`).
#' @param Comparison a character vector of length one; accepted values: `PxO` practice vs. outcome,`PxEU` practice vs. product (experimental unit) or
#' `OxEU` outcome vs. product. These are the respective x and y axes to be used in plotting.
#' @param CodeAxes a character vector of length two; accepted values: `Yes` or `No`; default = `c("Yes","Yes")`; When set to `Yes` axes show coded names,
#' when `No` axes show full names. Position 1 of this parameters refers to the x-axis and position 2 the y-axis of plots.
#' @param PLevel character vector of length one; accepted values:  `T` theme, `P` practice or `S` subpractice. This parameter indicates the **practice**
#' level for analysis.
#' @param OLevel character vector of length one; accepted values:  `P` pillar, `SP` subpillar, `I` indicator or `SI` subindicator. This parameter
#' indicates the **outcome** level for analysis.
#' @param EULevel character vector of length one; accepted values:  `T` type, `S` subtype  or `P` product. This parameter indicates the **product**
#' level for analysis. *Note products are also referred to as experimental units (EU) in ERA*.
#' @return `PrepareXY` returns a list with two elements:
#' * `[[Data]]` returns a `data.table` of the supplied dataset with focal columns renamed to `Axis1`, `Axis2` and `Group`.
#' * `[[Lab]]` returns a character vector of names corresponding to `Axis1`, `Axis2` and `Group`.
#' @export
#' @import data.table
PrepareXY<-function(Data,Comparison,CodeAxes =c("Yes","Yes"),PLevel,OLevel,EULevel){
  Data<-data.table(Data)
  Data<-StandColNames(Data,PLevel,OLevel,EULevel)

  ERA.Matrix.Axes<-data.table(Choice=c("PxO","PxEU","OxEU"),
                              Axis1 = c("Practice","Practice","Outcome"),
                              Axis2 = c("Outcome","Product","Product"),
                              Group = c("Product","Outcome","Practice"))


  Axis1<-ERA.Matrix.Axes[Choice==Comparison,Axis1]
  Axis2<-ERA.Matrix.Axes[Choice==Comparison,Axis2]
  Group<-ERA.Matrix.Axes[Choice==Comparison,Group]


  if(CodeAxes[1]=="Yes"){
    Axis1<-paste0(Axis1,".Code")
  }
  if(CodeAxes[2]=="Yes"){
    Axis2<-paste0(Axis2,".Code")
  }

  setnames(Data,Axis1,"Axis1")
  setnames(Data,Axis2,"Axis2")
  setnames(Data,Group,"Group")

  return(list(Data=Data,Labs=c(Axis1=Axis1,Axis2=Axis2,Group=Group)))
}
