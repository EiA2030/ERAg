#' Get EcoCrop parameters for ERA products
#'
#' Extracts ecocrop temperature and cycle length variables for a vector of product codes.
#
#' @param Products A character vector of products that match the `Product` field of the `EUCodes` dataset
#' @return A data.table is returned with fields:
#' * `species` = scientific name of crop (a unique identifier in the `ERA_EcoCrop` dataset)
#' * `Topt.low` = minimum of crop optimal temperature range - C
#' * `Topt.high` = maximum of crop optimal temperature range - C
#' * `Tlow` = minimum of crop absolute temperature range - C
#' * `Thigh` = maximum of crop absolute temperature range - C
#' * `cycle_min` = minimum of crop growing cycle duration range - days
#' * `cycle_max` = maximum of crop growing cycle duration range - days
#' @export
#' @import data.table
AddEcoCrop<-function(Products){

  E.Names<-data.table(EUCodes)[match(Products,Product),ECOCROP.Name]

  E.Data<-ERA_EcoCrop[match(E.Names,species),c("species","temp_opt_min","Temp_Opt_Max","Temp_Abs_Min","Temp_Abs_Max","cycle_min","cycle_max")]

  colnames(E.Data)[2:5]<-c("Topt.low","Topt.high","Tlow","Thigh")

  E.Data[,cycle_min:=as.numeric(cycle_min)]
  E.Data[,cycle_max:=as.numeric(cycle_max)]
  E.Data[,Topt.low:=as.numeric(Topt.low)]
  E.Data[,Topt.high:=as.numeric(Topt.high)]

  X<-E.Data[,Tlow]
  X<-unlist(lapply(X,FUN=function(X){
    if(is.null(X)){NA}else{X}
  }))
  E.Data[,Tlow:=as.numeric(X)]


  X<-E.Data[,Thigh]
  X<-unlist(lapply(X,FUN=function(X){
    if(is.null(X)){NA}else{X}
  }))

  E.Data[,Thigh:=as.numeric(X)]

  return(E.Data)
}
