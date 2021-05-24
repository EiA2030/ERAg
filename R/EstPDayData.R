#' Estimate missing planting dates from existing values in ERA
#'
#' For NA values of planting date in the dataset supplied the function attempts to substitute values for the same `Product`/`Product.Subtype` and growing season
#' which are spatially nearby.
#'
#' The function searches iteratively within five levels increasing distance corresponding to latitude and
#' longitude measured using 5 to 1 decimal places. If there is more than one planting date value for a `Product` x season (`M.Year`) combination then
#' values are averaged. If no corresponding planting dates are found matching on product then matching is attempted on `Product.Subtype` (e.g. cereals
#' or legumes).#'
#
#' @param DATA An ERA dataset (e.g. `ERA.Compiled`)
#' @return The following fields are appended to the input dataset:
#' `Data.PS.Date` = planting start date estimated from the dataset provided (Date)
#' `Data.PE.Date` = planting end date estimated from the dataset provided (Date)
#' `Data.Date.Acc` = a concantenation of the number of decimals places used in latitude/longitude matching and whether matching was conducted using
#' `Product` (`P.Sub`) or `Product.Subtype` (`P`), for example `3-P.Sub`.
#' @export
EstPDayData<-function(DATA){

  DATA<-data.table(DATA)

  if(any(c("Data.PS.Date","Data.PE.Date") %in% colnames(DATA))){
    DATA<-DATA[,!c("Data.PS.Date","Data.PE.Date")]
  }

  DATA[,Plant.Start:=as.Date(Plant.Start,"%d.%m.%Y")
  ][,Plant.End:=as.Date(Plant.End,"%d.%m.%Y")
  ]

  Exists<-unique(DATA[!is.na(Plant.Start),list(Plant.Start,Plant.End,Latitude,Longitude,M.Year,Product)])
  Exists<-Exists[,list(PS.Mean=mean(Plant.Start,na.rm=T),
                       PS.Median=as.Date(median(Plant.Start,na.rm=T)),
                       PE.Mean=mean(Plant.End,na.rm=T),
                       PE.Median=as.Date(median(Plant.End,na.rm=T)),
                       PS.N=length(!is.na(Plant.Start))),
                 by =list(Latitude,Longitude,M.Year,Product)]

  DATA[,Data.PS.Date:=as.Date(NA)][,Data.PE.Date:=as.Date(NA)]

  for(i in 5:1){
    Exists<-Exists[!is.na(PS.Median)][,Code:=paste(round(Latitude,i),round(Longitude,i),M.Year )]

    suppressWarnings(
      DATA[,MCode:=paste(round(Latitude,i),round(Longitude,i),M.Year,Product)
      ][is.na(Plant.Start) & is.na(Data.PS.Date),Data.Date.Acc:=paste0(i,"-P")
      ][is.na(Plant.Start)  & is.na(Data.PS.Date),Data.PS.Date:=Exists$PS.Median[match(MCode,Exists$Code)]
      ][is.na(Plant.Start)  & is.na(Data.PE.Date),Data.PE.Date:=Exists$PE.Median[match(MCode,Exists$Code)]]
    )
  }

  Exists<-unique(DATA[,list(Plant.Start,Plant.End,Latitude,Longitude,M.Year,Product.Subtype)])
  Exists<-DATA[,list(PS.Mean=mean(Plant.Start,na.rm=T),
                     PS.Median=as.Date(median(Plant.Start,na.rm=T)),
                     PE.Mean=mean(Plant.End,na.rm=T),
                     PE.Median=as.Date(median(Plant.End,na.rm=T)),
                     PS.N=length(!is.na(Plant.Start))),
               by =list(Latitude,Longitude,M.Year,Product.Subtype)]


  for(i in 5:1){
    Exists<-Exists[!is.na(PS.Median)][,Code:=paste(round(Latitude,i),round(Longitude,i),M.Year,Product.Subtype)]

    DATA[,MCode:=paste(round(Latitude,i),round(Longitude,i),M.Year,Product.Subtype)
    ][is.na(Plant.Start)  & is.na(Data.PS.Date),Data.Date.Acc:=paste0(i,"-P.Sub")
    ][is.na(Plant.Start)  & is.na(Data.PS.Date),Data.PS.Date:=Exists$PS.Median[match(MCode,Exists$Code)]
    ][is.na(Plant.Start)  & is.na(Data.PE.Date),Data.PE.Date:=Exists$PE.Median[match(MCode,Exists$Code)]]

  }


  DATA[,MCode:=NULL]



  return(DATA)

}
