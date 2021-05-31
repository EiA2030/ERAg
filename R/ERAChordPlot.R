#' Create Chord Plots
#'
#' The `ERAChordPlot` function plots the output of the `ERAg::PrepareChordData` function to visualise the relationship between ERA objects.
#'
#' This function uses the \link[circlize]{chordDiagram} to create the chord plot and also requires the `ComplexHeatmap` package to generate legends.
#' The latter can be installed using `devtools::install_github("jokergoo/ComplexHeatmap").`
#'
#' @param Chord.Data A `data.table` object output by the `ERAg::PrepareChordData`function.
#' @param Type A character vector of either `"Chord"` or `"Grid"`. If `Type="Grid"` chords are coloured by the `from` objects in the plot, if `Type="Chord"`
#' chords are colored by the field specified by `Value.Var`.
#' @param Alpha A numeric value between 0 and 1 indicating the transparency of chords
#' @param Value.Var A character vector of either `"yi"` or `"vote"`. If `Type="Chord"` then this argument chooses the value statistic that colours chords, `"yi"`
#' = response ratio and `"vote"` = vote count.
#' @param Value.Mid A numeric vector of length one; choose the mid-value of the colour palette, for response ratios (`Value.Var=="yi"`) this is typically 0.
#' @param Cont.Pallete A character vector of length two describing two `brewer.pal`palettes from`rownames(RColorBrewer::brewer.pal.info)[RColorBrewer::brewer.pal.info$category=="seq"])`.
#' Set to `NA` to use default palettes,
#' @param Legend.Tit  A character vector of length one; title of the legend.
#' @return Returns a \link[circlize]{chordDiagram} plot capture using `recordPlot()`.
#' @export
ERAChordPlot<-function(Chord.Data,
                       Type="Chord",
                       Alpha=0.5,
                       Value.Var="yi",
                       Value.Mid=0,
                       Cont.Pallete = NA,
                       Legend.Tit=""){

  if(any(Chord.Data[,to] %in% Chord.Data[,from])){
    Chord.Data[,to:=paste0(to," ")]
  }

  Chord.N.From<-Chord.Data[,length(unique(from))]
  Chord.Pal.From.Spectral<-colorRampPalette(RColorBrewer ::brewer.pal(8, "Dark2"))(Chord.N.From)
  Chord.Pal.From.Viridis<-viridisLite::viridis(Chord.N.From)
  Grid.Cols<-Chord.Pal.From.Spectral
  names(Grid.Cols)<-Chord.Data[,unique(from)]

  Chord.N.to<-Chord.Data[,length(unique(to))]
  Chord.Pal.to.Spectral<-colorRampPalette(RColorBrewer ::brewer.pal(9, "Set1"))(Chord.N.to)
  Chord.Pal.to.Viridis<-viridisLite:: viridis(Chord.N.to)
  Grid.Cols2<-Chord.Pal.to.Spectral

  Grid.Cols<-c(Grid.Cols,Grid.Cols2)

  if(Type == "Grid"){
    circlize::chordDiagram(Chord.Data[,c("from","to","value")],grid.col = Grid.Cols,transparency = Alpha)
  }

  if(Type=="Chord"){
    # Colour by chord = vote


    Chord.Data<-Chord.Data[order(Chord.Data[,..Value.Var],decreasing = T)]


    Low.Vals<- unlist(Chord.Data[unlist(Chord.Data[,..Value.Var])<Value.Mid,..Value.Var])
    High.Vals<- unlist(Chord.Data[unlist(Chord.Data[,..Value.Var])>=Value.Mid,..Value.Var])

    Max.Val<-max(c(Low.Vals,High.Vals)^2)^0.5

    if(is.na(Cont.Pallete[1])){
      # Here we create negative and positive palletes of length 1000, we then select from the pallete according to the effect size
      LowPal<-colorRampPalette(RColorBrewer ::brewer.pal(9, "YlOrRd"))(1000)
      Low<-LowPal[round(1000*((Low.Vals/Max.Val)^2)^0.5,0)]

      HighPal<-rev(colorRampPalette(RColorBrewer ::brewer.pal(9, "PuBuGn"))(1000))
      High<-HighPal[round(1000*((High.Vals/Max.Val)^2)^0.5,0)]
    }else{
      LowPal<-colorRampPalette(RColorBrewer ::brewer.pal(9, Cont.Pallete[1]))(1000)
      Low<-LowPal[round(1000*((Low.Vals/Max.Val)^2)^0.5,0)]

      HighPal<-rev(colorRampPalette(RColorBrewer ::brewer.pal(9, Cont.Pallete[2]))(1000))
      High<-HighPal[round(1000*((High.Vals/Max.Val)^2)^0.5,0)]

    }

    PAL<-c(High,Low)


    Low.Vals<-round(c(Value.Mid,-Max.Val/2,-Max.Val),2)
    High.Vals<-round(c(Value.Mid,Max.Val/2,Max.Val),2)



    circlize::chordDiagram(Chord.Data[,c("to","from","value")],grid.col = Grid.Cols,col = PAL,transparency = Alpha, annotationTrack = c("name","grid"))


    lgd_high = ComplexHeatmap::Legend(at = High.Vals,
                      col_fun = circlize::colorRamp2(High.Vals,HighPal[c(1000,500,1)]),
                      title_position = "topleft",
                      title = paste0(Legend.Tit," >=",Value.Mid))

    lgd_low = ComplexHeatmap::Legend(at = rev(Low.Vals),
                     col_fun = circlize::colorRamp2(rev(Low.Vals),LowPal[c(1000,500,1)]),
                     title_position = "topleft",
                     title = paste0(Legend.Tit," <",Value.Mid))

    ComplexHeatmap::draw(lgd_high, x = unit(0.1, "npc"), y = unit(0.30, "npc"), just = c("left", "bottom"))
    ComplexHeatmap::draw(lgd_low, x = unit(0.1, "npc"), y = unit(0.1, "npc"), just = c("left", "bottom"))
  }
  p <- recordPlot()
  return(p)

}
