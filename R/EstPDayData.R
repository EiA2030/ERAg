#' Estimate missing planting dates from existing values in ERA
#'
#' We may wish to substitute NA values of planting date in ERA with estimates derived from  same `Product/Product.Subtype`, year and
#' growing season which are spatially nearby, the `EstPDayData` functions generates these estimates.
#'
#' The function searches iteratively within five levels of increasing distance corresponding to latitude and longitude recorded from 5 to 1 decimal
#' places. If there is more than one planting date value for a `Product` x `M.Year.Start` x `M.Year.End` x `Season.Start` x `Season.End`
#' combination then values are averaged. If no corresponding planting dates are found matching on product then matching is attempted
#' on `Product.Subtype` (e.g. cereals or legumes).
#'
#' If Latitude + Longitude combinations have a mixture of season = 1 or 2 and season = NA values this suggests a potential issue
#' with the consistency of temporal recording at the spatial coordinates. Observations with such issues are excluded from the analysis and flagged in
#' the output `[[Season.Issues]]` list object.
#'
#' Observation with irrigation in the control and treatment are removed from consideration as these will not have realistic planting dates for rainfed areas.
#'
#' @param DATA An ERA dataset (e.g. `ERA.Compiled`)
#' @return A list of two elements is returned:
#' 1) `[[DATA]]` a data.table where the following fields are appended to the input dataset:
#' `Data.PS.Date` = planting start date estimated from the dataset provided (Date)
#' `Data.PE.Date` = planting end date estimated from the dataset provided (Date)
#' `Data.Date.Acc` = a concantenation of the number of decimals places used in latitude/longitude matching and whether matching was conducted using
#' `Product` (`P`) or `Product.Subtype` (`P.Sub`), for example `3-P.Sub`.
#' 2) `[[Season.Issues]]` a data.table containing Latitude + Longitude combinations where a mixture of season = 1 or 2 and season = NA are reported suggesting
#' potential issues with consistency of temporal recording at these spatial coordinates.
#' @export
#' @import data.table
EstPDayData<-function(DATA){

  DATA<-data.table(DATA)

  if(any(c("Data.PS.Date","Data.PE.Date") %in% colnames(DATA))){
    DATA<-DATA[,!c("Data.PS.Date","Data.PE.Date")]
  }

  DATA[,Plant.Start:=as.Date(Plant.Start,"%d.%m.%Y")
  ][,Plant.End:=as.Date(Plant.End,"%d.%m.%Y")
  ][,Data.PS.Date:=as.Date(NA)][,Data.PE.Date:=as.Date(NA)]

  # Find observations in the data with planting dates removing any observations that are irrigated in control and treatment.
  Exists<-unique(DATA[!is.na(Plant.Start) & !(Irrigation.C & Irrigation.T),list(Plant.Start,Plant.End,Latitude,Longitude,Product.Simple,M.Year.Start,M.Year.End,Season.Start,Season.End)])
  Exists<-Exists[,list(PS.Mean=mean(Plant.Start,na.rm=T),
                       PS.Median=as.Date(median(Plant.Start,na.rm=T)),
                       PE.Mean=mean(Plant.End,na.rm=T),
                       PE.Median=as.Date(median(Plant.End,na.rm=T)),
                       PS.N=length(!is.na(Plant.Start))),
                 by =list(Latitude,Longitude,Product.Simple,M.Year.Start,M.Year.End,Season.Start,Season.End)]

  for(i in 5:1){
    Exists<-Exists[!is.na(PS.Median)
    ][,LatRound:=round(Latitude,i)
    ][,LonRound:=round(Longitude,i)
    ][,Code:=paste(LatRound,LonRound,M.Year.Start,M.Year.End,Season.Start,Season.End,Product.Simple)]

    # If there is a mixture of with and without seasons at a location flag data and exclude
    Exists[,Season.Flag:=any(is.na(Season.Start)) & any(!is.na(Season.Start)),by=list(LatRound,LonRound)]

    if(i==5){
      Flagged.Product<-Exists[Season.Flag==T][,Data.Date.Acc:=paste0(i,"-P")]
    }else{
      A<-Exists[Season.Flag==T][,Data.Date.Acc:=paste0(i,"-P")]
      Flagged.Product<-rbind(Flagged.Product,A)
    }

    Exists<-Exists[Season.Flag==F]

    suppressWarnings(
      DATA[,MCode:=paste(round(Latitude,i),round(Longitude,i),M.Year.Start,M.Year.End,Season.Start,Season.End,Product.Simple)
      ][is.na(Plant.Start) & is.na(Data.PS.Date),Data.Date.Acc:=paste0(i,"-P")
      ][is.na(Plant.Start)  & is.na(Data.PS.Date),Data.PS.Date:=Exists$PS.Median[match(MCode,Exists$Code)]
      ][is.na(Plant.Start)  & is.na(Data.PE.Date),Data.PE.Date:=Exists$PE.Median[match(MCode,Exists$Code)]]
    )
  }

  Exists<-unique(DATA[,list(Plant.Start,Plant.End,Latitude,Longitude,M.Year.Start,M.Year.End,Season.Start,Season.End,Product.Subtype)])
  Exists<-DATA[,list(PS.Mean=mean(Plant.Start,na.rm=T),
                     PS.Median=as.Date(median(Plant.Start,na.rm=T)),
                     PE.Mean=mean(Plant.End,na.rm=T),
                     PE.Median=as.Date(median(Plant.End,na.rm=T)),
                     PS.N=length(!is.na(Plant.Start))),
               by =list(Latitude,Longitude,M.Year.Start,M.Year.End,Season.Start,Season.End,Product.Subtype)]


  for(i in 5:1){
    Exists<-Exists[!is.na(PS.Median)
    ][,LatRound:=round(Latitude,i)
    ][,LonRound:=round(Longitude,i)
    ][,Code:=paste(LatRound,LonRound,M.Year.Start,M.Year.End,Season.Start,Season.End,Product.Subtype)]

    # If there is a mixture of with and without seasons at a location flag data and exclude
    Exists[,Season.Flag:=any(is.na(Season.Start)) & any(!is.na(Season.Start)),by=list(LatRound,LonRound)]

    if(i==5){
      Flagged.Product.Subtype<-Exists[Season.Flag==T][,Data.Date.Acc:=paste0(i,"-P")]
    }else{
      A<-Exists[Season.Flag==T][,Data.Date.Acc:=paste0(i,"-P")]
      Flagged.Product.Subtype<-rbind(Flagged.Product.Subtype,A)
    }

    Exists<-Exists[Season.Flag==F]

    DATA[,MCode:=paste(round(Latitude,i),round(Longitude,i),M.Year.Start,M.Year.End,Season.Start,Season.End,Product.Subtype)
    ][is.na(Plant.Start)  & is.na(Data.PS.Date),Data.Date.Acc:=paste0(i,"-P.Sub")
    ][is.na(Plant.Start)  & is.na(Data.PS.Date),Data.PS.Date:=Exists$PS.Median[match(MCode,Exists$Code)]
    ][is.na(Plant.Start)  & is.na(Data.PE.Date),Data.PE.Date:=Exists$PE.Median[match(MCode,Exists$Code)]]

  }


  DATA[,MCode:=NULL]


  Flagged.Product[,EULevel:="Product.Simple"]
  Flagged.Product.Subtype[,EULevel:="Product.Subtype"]

  setnames(Flagged.Product,"Product.Simple","Product.Name")
  setnames(Flagged.Product.Subtype,"Product.Subtype","Product.Name")

  X<-rbind(Flagged.Product,Flagged.Product.Subtype,use.names = T)

  Y5<-unique(DATA[,LatLon:=paste(Latitude,Longitude)][,list(LatLon,Code)][,Code:=paste(unique(Code),collapse = "|"),by=LatLon])
  Y4<-unique(DATA[,LatLon:=paste(round(Latitude,4),round(Longitude,4))][,list(LatLon,Code)][,Code:=paste(unique(Code),collapse = "|"),by=LatLon])
  Y3<-unique(DATA[,LatLon:=paste(round(Latitude,3),round(Longitude,3))][,list(LatLon,Code)][,Code:=paste(unique(Code),collapse = "|"),by=LatLon])
  Y2<-unique(DATA[,LatLon:=paste(round(Latitude,2),round(Longitude,2))][,list(LatLon,Code)][,Code:=paste(unique(Code),collapse = "|"),by=LatLon])
  Y1<-unique(DATA[,LatLon:=paste(round(Latitude,1),round(Longitude,1))][,list(LatLon,Code)][,Code:=paste(unique(Code),collapse = "|"),by=LatLon])

  X[,LatLon5:=paste(round(Latitude,5),round(Longitude,5))]
  X[,Code5:=Y5[match(X[,LatLon5],LatLon),Code]]

  X[,LatLon4:=paste(round(Latitude,4),round(Longitude,4))]
  X[,Code4:=Y4[match(X[,LatLon4],LatLon),Code]]

  X[,LatLon3:=paste(round(Latitude,3),round(Longitude,3))]
  X[,Code3:=Y3[match(X[,LatLon3],LatLon),Code]]

  X[,LatLon2:=paste(round(Latitude,2),round(Longitude,2))]
  X[,Code2:=Y2[match(X[,LatLon2],LatLon),Code]]

  X[,LatLon1:=paste(round(Latitude,1),round(Longitude,1))]
  X[,Code1:=Y1[match(X[,LatLon1],LatLon),Code]]

  return(list(DATA=DATA,Season.Issues=X))

}
