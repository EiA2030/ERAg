#' Plot ERA Data Locations with Hex Density Layer
#'
#' This function plots the density of ERA observations over a map of Africa. A hexagonal density layer is overlain using the \link[ggplot2]{geom_hex} function and points can be displayed.
#'
#' The map of Africa is supplied using the  \link[rworldmap]{getMap} function.
#'
#' @param Data A data.frame or data.table with decimal degree point locations in columns named `Latitude` & `Longitude`. NA values are not permitted.
#' @param Low Colour of low data density areas. Default = `grey10`.
#' @param Mid Colour of medium data density areas. Default = `grey50`.
#' @param High Colour of high data density areas. Default = `black`.
#' @param Point.Col Colour of points. Default = `yellow`.
#' @param Do.Log A character vector of `Yes`  or `No`. If `Yes` the shading of hexagons uses logged counts as `stat(log(count))`. Default = `Yes`.
#' @param Showpoints A character vector of `Yes`  or `No`. If `Yes` point locations are plotted on the map. Default = `No`.
#' @param ALevel If `NA` then the dataset is subset to unique values of `Latitude` and `Longitude`. If a vector of column names is supplied then the dataset is subset to
#' unique values of `Latitude` and `Longitude` plus these columns. Default = `NA`.
#' @return ERAHexPlot returns a `ggplot` object showing a map of point density
#' @export
#' @importFrom sf st_combine st_as_sf
#' @importFrom rworldmap getMap
#' @import rworldxtra
#' @importFrom ggplot2 ggplot geom_hex scale_fill_gradient2 theme_bw theme geom_sf coord_sf aes element_blank element_text
#' @import data.table

ERAHexPlot<-function(Data,Low = "grey10",Mid = "grey50",High = "black",Point.Col = "yellow",Do.Log="Yes",Showpoints="No",ALevel=NA){
  Data<-data.table(Data)
  if(!is.na(ALevel)){
    Point.Data<-unique(Data[!(is.na(Latitude)|is.na(Longitude)),list(Latitude,Longitude,ALevel)])
  }else{
    Point.Data<-unique(Data[!(is.na(Latitude)|is.na(Longitude)),list(Latitude,Longitude)])
  }

  sites <- st_combine(st_as_sf(Point.Data, coords = c("Longitude", "Latitude"),
                               crs = 4326, agr = "constant"))

  AfricaMap<-getMap(resolution = "high")
  AfricaMap<-AfricaMap[AfricaMap$REGION=="Africa"&!is.na(AfricaMap$REGION),]
  AfricaMap <-st_as_sf(AfricaMap,crs = 4326)

  g<-ggplot(sites)

  if(Do.Log=="Yes"){
    g<-g+geom_hex(data=Point.Data,aes(x = Longitude, y = Latitude,fill = stat(log(count))))+
      scale_fill_gradient2("log(No. Studies)",low=Low,mid=Mid,high=High)
  }else{
    g<-g+geom_hex(data=Point.Data,aes(x = Longitude, y = Latitude))+
      scale_fill_gradient2("No. Studies",low=Low,mid=Mid,high=High)
  }
  g<-g+theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = c(0.16,0.27),
          legend.background = element_blank(),
          legend.text = element_text(colour = "black",face="bold"),
          legend.title = element_text(colour = "black",face="bold"))
  if(Showpoints=="Yes"){
    g<-g+geom_sf(data=sites,size = 1, shape = 16,colour=Point.Col,alpha=1)
  }

  g<-g+geom_sf(data=AfricaMap,aes(x=LON,y=LAT),fill=NA,col="black")+
    coord_sf(xlim=c(-25,65),ylim=c(-35,35))

  return(g)
}
