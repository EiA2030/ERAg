#' Data Availability Matrix Plot
#'
#' Plots a matrix shaded for data availability using the \link[ggplot]{geom_tile} function for an object returned from the `PrepareXY` function.
#'
#' The fill of and numbers in the matrix tiles are the number of rows for each combination of values in `Axis1` and `Axis2` in the `Data` element of the
#' list returned by `ERA::PrepareXY`.
#'
#' Note `PrepareXY` selects of two of the three higher order ERA concepts of  **practice**, **outcome** and **product**, counts for the selected concepts
#' include all levels of the non-selected concept. To explore results within in a non-selected concept you will need to subset the `Data` element of the
#' list returned by `PrepareXY` on the `Group` column.
#'
#' @param Data a `list` returned from the from the `ERA::PrepareXY` function
#' @param DoLog A character vector of length one; accepted values: `Yes`  or `No`; default = `No`; if `Yes` fill values are natural log transformed.
#' @param Palette A character vector of length one; accepted values: `magma`, `inferno`, `plasma` or `viridis`; default = `plasma`; sets the palette of
#' of the matrix fill using the \link[ggplot]{scale_fill_viridis} function
#' @param FlipXY A character vector of length one; accepted values: `Yes`  or `No`; default = `No`; if `Yes` the x and y axes of the plot are swapped
#' @param RotYlab A character vector of length one; accepted values: `Yes`  or `No`; default = `Yes`; if `Yes` the y axis labels of the plot are rotated 90 degrees
#' @param ALevel A character vector of length one that labels the legend of the plot, this should refer to any data aggregation by study or location that has occurred (or not); default = `Observation`
#' @return `ERAMatrixPlot` returns a  \link[ggplot]{geom_tile} plot
#' @export
#' @import data.table
#' @importFrom ggplot2 ggplot aes geom_tile geom_text coord_flip theme
#' @importFrom viridis scale_fill_viridis
ERAMatrixPlot<-function(Data,DoLog = "No",Palette = "plasma",FlipXY = "No",RotYlab = "Yes",ALevel = "Observation"){

PlotData<-Data[[1]][,list(value=.N),by=list(Axis1,Axis2)]
Labs<-Data[[2]]


if(DoLog=="Yes"){
  g<-ggplot(PlotData,aes(x=Axis1,y=Axis2,label=value))+
    geom_tile(aes(fill = log(value)), colour = "black")+
    scale_fill_viridis(paste0("ln(",ALevel,")"),direction=-1,alpha=0.5,option=Palette)

}else{
  g<-ggplot(PlotData,aes(x=Axis1,y=Axis2,label=value))+
    geom_tile(aes(fill = value), colour = "black")+
    scale_fill_viridis(ALevel,direction=-1,alpha=0.5,option=Palette)

}

g<-g+geom_text()+
  theme_bw()+
  labs(x=Labs[1],
       y=Labs[2])

if(FlipXY=="Yes"){
  g<-g+coord_flip()
}

if(RotYlab=="Yes"){
  g<-g+theme(axis.text.x = element_text(angle=90))
}

return(g)
}
