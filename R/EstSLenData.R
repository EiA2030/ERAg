#' Estimate missing season lengths from existing values in ERA
#'
#' We may wish to substitute NA values of season length (i.e. harvest date is unknown) in ERA with estimates derived from  same `Product/Product.Subtype`, year and
#' growing season which are spatially nearby, the `EstPDayData` functions generates these estimates.
#'
#' Season length is calculated as the mean difference between mean harvest date (midpoint of `Harvest.Start` and `Harvest.End` Date columns) and mean planting date
#' (midpoint of `Plant.Start` and `Plant.End` Date columns).
#'
#' The function searches iteratively within five levels of increasing distance corresponding to latitude and longitude recorded from 5 to 1 decimal
#' places. If there is more than one season length value for a `Product` x season (`M.Year`) combination then values are averaged. If no corresponding
#' season lengths are found matching on product then matching is attempted on `Product.Subtype` (e.g. cereals or legumes).
#
#' @param DATA An ERA dataset (e.g. `ERA.Compiled`)
#' @return The following fields are appended to the input dataset:
#' `Data.SLen` = season length date estimated from the dataset provided (days)
#' `Data.SLen.Acc` = a concantenation of the number of decimals places used in latitude/longitude matching and whether matching was conducted using
#' `Product` (`P.Sub`) or `Product.Subtype` (`P`), for example `3-P.Sub`.
#' @export
#' @import data.table
EstSLenData<-function(DATA){

if(any("Data.SLen" %in% colnames(DATA))){
  DATA<-DATA[,!"Data.SLen"]
}

DATA[,Harvest.Start:=if(!class(Harvest.Start)=="Date"){as.Date(Harvest.Start,"%d.%m.%Y")}else{Harvest.Start}
][,Harvest.End:=if(!class(Harvest.End)=="Date"){as.Date(Harvest.End,"%d.%m.%Y")}else{Harvest.End}
][,Plant.Start:=if(!class(Plant.Start)=="Date"){as.Date(Plant.Start,"%d.%m.%Y")}else{Plant.Start}
][,Plant.Start:=if(!class(Plant.Start)=="Date"){as.Date(Plant.Start,"%d.%m.%Y")}else{Plant.Start}
][,SLen:=(as.numeric(Harvest.Start)+((as.numeric(Harvest.End)-as.numeric(Harvest.Start))/2))-(as.numeric(Plant.Start)+((as.numeric(Plant.End)-as.numeric(Plant.Start))/2))
][,Data.SLen.Acc:=as.character(NA)
][,Data.SLen:=as.numeric(NA)]

Exists<-unique(DATA[!is.na(SLen),c("SLen","Latitude","Longitude","M.Year","Product")])

Exists<-Exists[,list(
  SL.Mean=as.numeric(mean(SLen,na.rm=T)),
  SL.Median=as.numeric(median(SLen,na.rm=T)),
  SL.N=as.numeric(length(!is.na(SLen)))
),
by =c("Latitude","Longitude","M.Year","Product")]



for(i in 5:1){
  Exists<-Exists[,Code:=paste(round(Latitude,i),round(Longitude,i),M.Year,Product)]

  DATA[,MCode:=as.character(paste(round(Latitude,i),round(Longitude,i),M.Year,Product))
  ][is.na(SLen) & is.na(Data.SLen),Data.SLen.Acc:=as.character(paste0(i,"-P"))
  ][is.na(SLen)  & is.na(Data.SLen),Data.SLen:=as.numeric(Exists$SL.Median[match(MCode,Exists$Code)])
  ][is.na(Data.SLen),Data.SLen.Acc:=NA]
}


DATA$MCode<-NULL

return(DATA)

}
