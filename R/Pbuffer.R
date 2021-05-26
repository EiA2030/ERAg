#' Buffer Points
#'
#' Creates circular buffers from a table containing point locations and buffer radii. The input table requires `Latitude` and `Longitude` columns in
#' decimal degrees and a numeric column named `Buffer` which indicates the buffer radius in meters.
#'
#' An optional `ID` field can be specified if there is need to separate any points with identical locations and buffers.
#'
#' @param Data A data.table or data.frame with decimal degree point locations in columns `Latitude` and `Longitude`, and a numeric column `Buffer` which indicates the buffer radius
#' to buffer each point in meters. NA values are not permitted and are filtered from the dataset.
#' @param ID The column name of any grouping variables used to split point x buffer locations. Default = NA.
#' @param Projected `Logical TRUE/FALSE`. If `TRUE` the output object has a projected CRS `+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs`,
#' if `FALSE` it has a geographic CRS `epsg:4326`.
#' @return Pbuffer returns circular buffers in an object of class `SpatialPolygons`.
#' @export
Pbuffer<-function(Data,ID=NA,Projected=F){
  Data<-data.frame(Data)
  Data<-Data[!(is.na(Data$Latitude)|is.na(Data$Longitude)|Data$Buffer==""|is.na(Data$Buffer)),]
  if(!is.na(ID)){
    SS<-unique(Data[,c("Latitude","Longitude",ID,"Buffer")])
  }else{
    SS<-unique(Data[,c("Latitude","Longitude","Buffer")])
  }
  # CRS of Site co-ordinates (lat/long)
  CRS.old<- "+init=epsg:4326"
  # CRS when creating buffers in meters (WGS 1984 World Mercator)
  CRS.new<-"+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

  # Buffer points
  points <- sp::SpatialPoints(cbind(SS$Longitude, SS$Latitude),proj4string=CRS(CRS.old))
  points <- sp::spTransform(points, CRS.new)

  pbuf1<-lapply(1:nrow(SS),FUN=function(i){
    pbuf<- rgeos::gBuffer(points[i], widt=SS$Buffer[i])
    pbuf<- sp::spChFIDs(pbuf, paste(i, row.names(pbuf), sep="."))
  })

  rownames(SS)<-lapply(pbuf1,names)

  pbuf1<-sp::SpatialPolygons(lapply(pbuf1, function(x){x@polygons[[1]]}),proj4string=CRS(CRS.new))

  pbuf1<-sp::SpatialPolygonsDataFrame(pbuf1,data = SS)

  if(Projected==T){
    return(pbuf1)
  }else{
    pbuf1<-sp::spTransform(pbuf1,CRS(CRS.old))
    return(pbuf1)
  }
}
