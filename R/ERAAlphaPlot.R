#' Plot ERA Data Locations with Alpha Density Layer
#'
#' This function plots the locations of ERA observations on a map of Africa, with a supplied background raster. A density raster is overlain which is transparent in high density areas (many ERA observations) and opaque in low density areas (few ERA observations). The bandwidth of alpha in the density raster can be adjusted using the `alpha.bandwidth`
#' parameter.
#'
#' The map of Africa is supplied using the  \link[rworldmap]{getMap} function.
#' @param Data A data.frame or data.table with decimal degree point locations in columns named `Latitude` & `Longitude`. NA values are not permitted.
#' @param Background A raster file that forms the background of the map. This file is cropped to the extent of Africa. A default farming system map is supplied from the
#' \href{https://doi.org/10.7910/DVN/G4TBLF/7RIFT5}{CELL5M} dataset.
#' @param Background.Labs A vector of names that correspond to the background raster values (in numerical order of the raster values).
#' @param Background.Cols A vector of colours that correspond to the background raster values (in numerical order of the raster values).
#' @param Background.Title The name of the background raster.
#' @param alpha.bandwidth A multiplicative bandwidth adjustment for use with the \link[ggplot2]{geom_density_2d} function. Higher values increase the size of the transparent area around
#' high data density regions.
#' @param Showpoints Logical `T/F`. If `TRUE` points locations are added to the plot.
#' @param Low Colour of low data density areas. Default = `black`.
#' @param Mid Colour of medium data density areas. Default = `grey30`.
#' @param High Colour of high data density areas. Default = `white`.
#' @param Point.Col Colour of points. Default = `black`.
#' @param ALevel If `NA` then the dataset is subset to unique values of `Latitude` and `Longitude`. If a vector of column names is supplied then then the dataset
#' is subset to unique values of `Latitude` and `Longitude` plus these columns. Default = `NA`.
#' @return AlphaD.Plot returns a `ggplot` object showing a map of point data density.
#' @export
#' @importFrom sf st_combine st_as_sf
#' @importFrom rworldmap getMap
#' @import rworldxtra
#' @importFrom ggplot2 ggplot scale_fill_manual theme_bw theme geom_sf coord_sf aes element_blank element_text geom_tile stat_density_2d scale_fill_gradient2 geom_point
#' @importFrom ggnewscale new_scale
#' @importFrom raster crop
#' @import data.table
ERAAlphaPlot<-function(Data = ERA.Compiled,
                      Background = NA,
                      Background.Labs = NA,
                      Background.Cols = NA,
                      Background.Title = NA,
                      alpha.bandwidth = 4,
                      Showpoints = T,
                      Low = "black",
                      Mid = "grey30",
                      High = "white",
                      Point.Col = "Black",
                      ALevel = NA){

  AfricaMap<-rworldmap::getMap(resolution = "high")
  AfricaMap<-AfricaMap[AfricaMap$REGION=="Africa"&!is.na(AfricaMap$REGION),]
  AfricaMap <-sf::st_as_sf(AfricaMap,crs = 4326)

  if(class(Background)[1]!="logical"){
    Background<-raster::crop(Background,AfricaMap)
  }

  # See below for use of new_scale("fill")
  # https://eliocamp.github.io/codigo-r/2018/09/multiple-color-and-fill-scales-with-ggplot2/


  # Sf causes a huge slow down when plotting points:
  # https://github.com/tidyverse/ggplot2/issues/2718 - potential fix using st_combine``

  if(!is.na(ALevel[1])){
    Point.Data<-unique(Data[,list(Latitude,Longitude,ALevel)])
  }else{
    Point.Data<-unique(Data[,list(Latitude,Longitude)])
  }

  if(class(Background)[1]!="logical"){
    g<-gplot(Background) +
    geom_tile(aes(fill = factor(value)))+
    scale_fill_manual(Background.Title,values=Background.Cols,labels=Background.Labs,na.value="white")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = c(0.18,0.29),
          legend.background = element_blank(),
          legend.text = element_text(colour = "white",face="bold"),
          legend.title = element_text(colour = "white",face="bold"),
          panel.border = element_blank()
    )+
    geom_sf(data=AfricaMap,aes(x=LON,y=LAT),fill=NA,col="black")+
    coord_sf(xlim=c(-26,65),ylim=c(-35,37.5))+
    new_scale("fill") +
    stat_density_2d(data = Data,
                    mapping = aes( x = Longitude, y = Latitude,fill = ..density.., alpha = (-..density..)), geom = "raster",
                    contour = FALSE,adjust=alpha.bandwidth,show.legend = F)+
    scale_fill_gradient2(low=Low,mid=Mid,high=High)
  }else{
    g<- ggplot(Data, aes(x = Longitude, y = Latitude)) +
      geom_sf(data=AfricaMap,aes(x=LON,y=LAT),fill=NA,col="black")+
      coord_sf(xlim=c(-26,65),ylim=c(-35,37.5))+
      stat_density_2d(data = Data,
                      mapping = aes( x = Longitude, y = Latitude,fill = ..density.., alpha = (-..density..)), geom = "raster",
                      contour = FALSE,adjust=alpha.bandwidth,show.legend = F)+
      scale_fill_gradient2(low=Low,mid=Mid,high=High)+
      theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position = c(0.18,0.29),
            legend.background = element_blank(),
            legend.text = element_text(colour = "white",face="bold"),
            legend.title = element_text(colour = "white",face="bold"),
            panel.border = element_blank()
      )
  }

  if(Showpoints){
    g<- g + geom_point(data=Point.Data,aes( x = Longitude, y = Latitude),size=0.8,pch=16,col=Point.Col)
  }

  return(g)
}
