#' Extract data from all tif files for a supplied vector layer
#'
#' If parallel processing can be added then speeds can be increased. See https://github.com/rspatial/terra/issues/36 using pack and unpack functions to
#' make serializable SpatRaster and SpatVector objects.
#' With a small modification on (line 97) it should be possible to extract using a raster too.
#'
#' @param FileDirectory the location of the folder containing `.tif` rasters to be extracted, this folder should only contain `.tif` files.
#' @param MaxChunkSize the maximum number of rasters to be stacked when extracting data, consider your working RAM when making this decision.
#' @param ExtractBy a set of locations in `SpatVector` form.
#' @param ID.ExtractBy A character vector providing names for each location in `ExtractBy`. The vector must be the same length and order as `ExtractBy`.
#' @param ID.ExtractBy.Name the name of the location field. Default = `ID.ExtractBy`.
#' @param ID.Target A character vector providing names identifying each raster in the `FileDirectory` folder. The vector must be the same length and order as the files in that folder.
#' @param ID.Target.Name  the name of the raster identity field. Default = `variable`.
#' @param Function the function to be applied to summarize values extracted for each location. If set to `NA` then no function is applied. Default = `mean`.
#' @param value.name  the name of the value field. Default = `value`.
#' @param NAValue if NA values are encoded with specific value (e.g. -9999) use this field to state that value.
#' @param Round an integer indicating the number of decimal places to round the value field to.
#' @return A data.table of daily precipitation data including the fields:
#' * `value.name` = values contained within the supplied rasters
#' * `ID.Target.Name` = identity of supplied rasters
#' * `ID.ExtractBy.Name` = identity of extraction location
#' @export
#' @import data.table
#' @importFrom terra rast extract classify project crs
#' @importFrom miceadds load.Rdata2
#' @importFrom pbapply pblapply
#' @importFrom stringr str_replace_all
ExtractFolder<-function(FileDirectory,
                        MaxChunkSize,
                        ExtractBy,
                        ID.Target,
                        ID.Target.Name="variable",
                        ID.ExtractBy,
                        ID.ExtractBy.Name=NA,
                        Function=mean,
                        value.name="value",
                        NAValue=NA,
                        Round=NA){

  if(length(ID.ExtractBy)!=length(ExtractBy)){
    stop("Error: length(ID.ExtractBy) != length(ExtractBy)")
  }

  Files<-list.files(FileDirectory,full.names = T)
  Files2<-gsub("[.]tif","",list.files(FileDirectory,full.names = F))
  Files2<-stringr::str_replace_all(Files2, "[[:punct:]]", ".")

  if(length(ID.Target)!=length(Files)){
    stop("Error: length(ID.Target) != length(list.files(FileDirectory,full.names = T))")
  }

  if(sum(!grepl("[.]tif",Files))>0){
    stop("Error: Non .tif files in FileDirectory")
  }


  File.Size<-mean(file.size(Files)/10^9)

  Chunks<-split(Files, ceiling(seq_along(1:length(Files))/MaxChunkSize))

  ExtractBy<-terra::project(ExtractBy,terra::crs(terra::rast(Files[1])))

  Extracted<-rbindlist(
    pblapply(Chunks,FUN=function(Chunk){
      Data<-terra::rast(Chunk)
      if(!is.na(NAValue)){
        Data<-terra::classify(Data,matrix(c(NAValue,as.numeric(NA)),ncol=2))
      }
      if(!suppressWarnings(is.na(Function))){
        Data<-data.table(terra::extract(Data,ExtractBy,fun=Function,na.rm=T))
      }else{
        Data<-data.table(terra::extract(Data,ExtractBy))
      }
      melt(Data,id.vars="ID")
    })
  )

  if(!is.na(Round)){
    Extracted[,value:=round(value,Round)]
  }

  # Change value name
  if(value.name!="value"){
    setnames(Extracted,"value",value.name)
  }

  # Add target data ID.Target
  ID.Target<-data.table(ID.Target=ID.Target,variable=Files2)

  Extracted<-merge(Extracted,ID.Target,by="variable",all.x=T)[,variable:=NULL]

  if(!is.na(ID.Target.Name)){
    setnames(Extracted,"ID.Target",ID.Target.Name)
  }

  # Add ExtractBy feature names
  ID.ExtractBy<-data.table(ID.ExtractBy=ID.ExtractBy)[,ID:=1:.N]

  Extracted<-merge(Extracted,ID.ExtractBy,by="ID",all.x=T)[,ID:=NULL]

  if(!is.na(ID.ExtractBy.Name)){
    setnames(Extracted,"ID.ExtractBy",ID.ExtractBy.Name)
  }

  return(Extracted)


}

