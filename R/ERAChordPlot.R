#' Create Chord Plots
#'
#' The `ERAChordPlot` function plots the output of the `ERAg::PrepareChordData` function to visualise the relationship between ERA objects.
#'
#' This function uses the \link[circlize]{chordDiagram} to create the chord plot and also requires the `ComplexHeatmap` package to generate legends.
#' The latter can be installed using `devtools::install_github("jokergoo/ComplexHeatmap").`
#'
#' @param Chord.Data A `data.table` object output by the `ERAg::PrepareChordData`function
#' @param Type A character vector of either `"Chord"`,`"Grid"` or `"Category"`. If `Type="Grid"` chords are coloured by the `from` objects in the plot, if `Type="Chord"`
#' chords are colored by continuous numeric field specified by `Value.Var`, and if `Type="Category"`chords are coloured by a categorical variable.
#' @param Alpha A numeric value between 0 and 1 indicating the transparency of chords
#' @param Value.Var A character vector of either `"yi"`,`"vote"` or `"cat"`. If `Type="Chord"` then this argument chooses the value statistic that colours chords, `"yi"`
#' = response ratio and `"vote"` = vote count. If `Type="Category"` then this argument should be set to `"cat"`; a column of class `character` in `Chord.Data` containing a categorical variable.
#' @param Value.Mid A numeric vector of length one; choose the mid-value of the colour palette, for response ratios (`Value.Var=="yi"`) this is typically 0
#' @param Cont.Pallete A character vector of length two describing two `brewer.pal`palettes from`rownames(RColorBrewer::brewer.pal.info)[RColorBrewer::brewer.pal.info$category=="seq"])`.
#' Set to `NA` to use default palettes
#' @param Legend.Tit  A character vector of length one; title of the legend
#' @param Facing a character vector of one; adjust facing of text: c("inside", "outside", "reverse.clockwise", "clockwise","downward", "bending", "bending.inside", "bending.outside")
#' @param Lab.Adjust  A numeric vector of length one; adjust the spacing between rotated labels and the track
#' @param Axis.Cex  A numeric vector of length one; adjusts the size of track axis labels
#' @param Lab.Cex  A numeric vector of length one; adjusts the size of sector labels
#' @param Lab.Font A interger for font style, see \link[graphics]{par}. 1 corresponds to plain text (the default), 2 to bold face, 3 to italic and 4 to bold italic.
#' @param Reduce A numeric vector of length one; a proportion between 0-1, chords widths below this proportion are excluded from the plot
#' @param Cat.Pal A character vector of length = `length(unique(Chord.Data$cat))`; the vector should contain colour values corresponding to the values in
#' @param niceFacing Logical `T/F`. Should the facing of text be adjusted to fit human eyes?
#' @param Show.Axis.Labels Logical `T/F`. Show axis labels?
#' @param Plot.Title A character vector of length one; text to display as the plot title.
#' @param Grid.Pal.From A character vector of colours recognized by `circlize::chordDiagram` that colours the **from** sectors of the chord plot
#' @param Grid.Pal.To A character vector of colours recognized by `circlize::chordDiagram` that colours the **to** sectors of the chord plot
#' @param Legend.xy.cat A numeric vector of length two; override the x and y position of the legend (units are npc)
#' @param Legend.xy.chord A numeric vector of length four; override the x and y position of the legend (units are npc), first two values adjust the high values and last two values the low values.
#' `Chord.Data$cat` which should form the vector names, for example  `Cat.Pal=setNames(c("blue", "green","Red"), c("A", "B","C"))`.
#' @return Returns a \link[circlize]{chordDiagram} plot capture using `recordPlot()`.
#' @export
#' @importFrom circlize chordDiagram circos.trackPlotRegion circos.text circos.axis
#' @importFrom RColorBrewer brewer.pal
#' @import data.table
ERAChordPlot<-function(Chord.Data,
                       Type="Chord",
                       Alpha=0.5,
                       Value.Var="yi",
                       Value.Mid=0,
                       Cont.Pallete = NA,
                       Legend.Tit="",
                       Facing="clockwise",
                       Lab.Adjust=0,
                       Lab.Cex=0.2,
                       Lab.Font=1,
                       Axis.Cex=0.3,
                       Reduce=0.01,
                       Cat.Pal=NA,
                       niceFacing=T,
                       Show.Axis.Labels=T,
                       Plot.Title=NA,
                       Grid.Pal.From=RColorBrewer ::brewer.pal(8, "PiYG"),
                       Grid.Pal.To=RColorBrewer ::brewer.pal(8, "BrBG"),
                       Legend.xy.cat=NA,
                       Legend.xy.chord=NA){

  if(any(Chord.Data[,to] %in% Chord.Data[,from])){
    Chord.Data[,to:=paste0(to," ")]
  }


  Chord.N.From<-Chord.Data[,length(unique(from))]
  Grid.Cols<-colorRampPalette(Grid.Pal.To)(Chord.N.From)
  names(Grid.Cols)<-Chord.Data[,unique(from)]

  Chord.N.to<-Chord.Data[,length(unique(to))]
  Grid.Cols2<-colorRampPalette(Grid.Pal.From)(Chord.N.to)
  names(Grid.Cols2)<-Chord.Data[,unique(to)]

  Grid.Cols<-c(Grid.Cols,Grid.Cols2)
  Grid.Cols[grepl("Other",names(Grid.Cols))]<-"Grey70"

  if(Type == "Grid"){
    circlize::chordDiagram(Chord.Data[,c("from","to","value")],grid.col = Grid.Cols,transparency = Alpha,reduce=Reduce)
  }

  if(Type=="Chord"){

    Chord.Data<-Chord.Data[order(Chord.Data[,..Value.Var],decreasing = T)]


    Low.Vals<- unlist(Chord.Data[unlist(Chord.Data[,..Value.Var])<Value.Mid,..Value.Var])
    High.Vals<- unlist(Chord.Data[unlist(Chord.Data[,..Value.Var])>=Value.Mid,..Value.Var])

    Max.Val<-max(c(Low.Vals,High.Vals)^2)^0.5

    if(is.na(Cont.Pallete[1])){
      # Here we create negative and positive palletes of length 1000, we then select from the pallete according to the effect size
      LowPal<-colorRampPalette(RColorBrewer ::brewer.pal(9, "YlOrRd"))(1000)
      Low<-LowPal[round(1000*((Low.Vals/Max.Val)^2)^0.5,0)]

      HighPal<-colorRampPalette(RColorBrewer ::brewer.pal(9, "PuBuGn"))(1001)
      High<-HighPal[round(1000*((High.Vals/Max.Val)^2)^0.5,0)+1]
    }else{
      LowPal<-colorRampPalette(RColorBrewer ::brewer.pal(9, Cont.Pallete[1]))(1000)
      Low<-LowPal[round(1000*((Low.Vals/Max.Val)^2)^0.5,0)]

      HighPal<-colorRampPalette(RColorBrewer ::brewer.pal(9, Cont.Pallete[2]))(1001)
      High<-HighPal[round(1000*((High.Vals/Max.Val)^2)^0.5,0)+1]

    }

    PAL<-c(High,Low)
    Low.Vals<-round(c(Value.Mid,-Max.Val/2,-Max.Val),2)
    High.Vals<-round(c(Value.Mid,Max.Val/2,Max.Val),2)

    circlize::chordDiagram(Chord.Data[,c("to","from","value")],
                           preAllocateTracks = 1,
                           annotationTrack = "grid",
                           grid.col = Grid.Cols,
                           col = PAL,
                           transparency = Alpha,
                           reduce=Reduce)


    lgd_high = ComplexHeatmap::Legend(at = High.Vals,
                                      col_fun = circlize::colorRamp2(High.Vals,HighPal[c(1,500,1001)]),
                                      title_position = "topleft",
                                      title = paste0(Legend.Tit," >=",Value.Mid))

    lgd_low = ComplexHeatmap::Legend(at = rev(Low.Vals),
                                     col_fun = circlize::colorRamp2(rev(Low.Vals),LowPal[c(1000,500,1)]),
                                     title_position = "topleft",
                                     title = paste0(Legend.Tit," <",Value.Mid))

    if(is.na(Legend.xy.chord[1])){
      Legend.xy.chord<-c(0.1,0.3,0.1,0.1)
    }


    ComplexHeatmap::draw(lgd_high, x = unit(Legend.xy.chord[1], "npc"), y = unit(Legend.xy.chord[2], "npc"), just = c("left", "bottom"))
    ComplexHeatmap::draw(lgd_low, x = unit(Legend.xy.chord[3], "npc"), y = unit(Legend.xy.chord[4], "npc"), just = c("left", "bottom"))
  }

  if(Type=="Category"){

    if(is.na(Legend.xy.cat[1])){
      Legend.xy.cat<-c(0.03,0.06)
    }

    PAL<-Cat.Pal[match(unlist(Chord.Data[,..Value.Var]),names(Cat.Pal))]

    circlize::chordDiagram(Chord.Data[,c("to","from","value")],
                           preAllocateTracks = 1,
                           annotationTrack = "grid",
                           grid.col = Grid.Cols,
                           col = PAL,
                           transparency = Alpha,
                           reduce=Reduce)


    lgd = ComplexHeatmap::Legend(labels = names(Cat.Pal),
                                 legend_gp = gpar(fill=Cat.Pal),
                                 title = Legend.Tit,
                                 title_position = "leftcenter",
                                 ncol=1)
    ComplexHeatmap::draw(lgd,
                         x = unit(Legend.xy.cat[1], "npc"),
                         y = unit(Legend.xy.cat[2], "npc"),
                         just = c("left", "bottom"))



  }


  circlize::circos.trackPlotRegion(ylim= c(0,1),track.index = 1, panel.fun = function(x, y) {
    circlize::circos.text(x = CELL_META$xcenter,
                y = CELL_META$cell.ylim[1] + Lab.Adjust,
                CELL_META$sector.index,
                facing = Facing,
                niceFacing = niceFacing,
                adj = c(0, 0.5),
                cex = Lab.Cex,
                font=Lab.Font)
  }, bg.border = NA)

  if(Show.Axis.Labels==T){
    circlize::circos.trackPlotRegion(ylim= c(0,1),track.index = 1, panel.fun = function(x, y) {
      circlize::circos.axis(h = "top",
                  major.tick.length = 0.2,
                  sector.index =  CELL_META$sector.index,
                  track.index = 2,
                  labels.cex = Axis.Cex)
    }, bg.border = NA)
  }

  if(!is.na(Plot.Title)){
    title(main=Plot.Title,adj=0)
  }

  p <- recordPlot()
  return(p)

}


