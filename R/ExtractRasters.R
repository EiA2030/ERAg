#' Extract data from multiple rasters or zipped rasters in a single operation
#'
#' For a dataset containing point locations with a buffer of spatial uncertainty this function averages raster values for a
#' character vector pointing to the location of raster files.
#'
#' Extraction uses the extract function of the terra package.
#'
#' @param Data A data.table or data.frame containing decimal degree point locations as two numeric columns `Latitude` `Longitude`, an numeric field
#' `Buffer` that describes a radius of spatial uncertainty in meters, and a unique id field naming each location whose name is specified using the `ID` parameter.
#' @param ID A character vector of length one containing the column name for a unique id field naming each location in the dataset provided.
#' @param Files A character vector listing the paths to raster files.
#' @param Save_Dir A character vector of length one containing the path to the directory where the extracted data are to be saved. Set to NA if you do not want to save the returned dataset.
#' @param Save_Name A character vector of length one containing a filename for the returned data.table. Data are saved as a '.RData' object.
#' @return A data.table is returned. For each raster specified in the `Files` argument columns are created using the raster filename with the following
#' suffixes:
#' * `Mean` = average of non-NA cells for each locations bounding box
#' * `SD` = standard deviation of non-NA cells for each locations bounding box
#' * `Median` = median of non-NA cells for each locations bounding box
#' * `Mode` = mode of non-NA cells for each locations bounding box
#' * `N.Cells` = total number of cells contained each locations bounding box
#' * `NA.Cells` = number of NA cells contained each locations bounding box
#' @export
#' @import data.table
#' @importFrom terra rast vect extract crs
#' @importFrom sp spTransform CRS
#' @importFrom data.table fread fwrite
ExtractRasters<-function(Data,
                         ID,
                         Files,
                         Save_Dir,
                         Save_Name){

  if(!is.na(Save_Dir) & substr(Save_Dir,nchar(Save_Dir),nchar(Save_Dir))!="/"){
    Save_Dir<-paste0(Save_Dir,"/")
  }


  # Make Mode function
  getmode <- function(v) {
    v<-v[!is.na(v)]
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  Data<-as.data.frame(Data)
  SS<-data.table(unique(Data[!(is.na(Data$Latitude)|is.na(Data$Latitude)|is.na(Data$Buffer)),c(ID,"Latitude","Longitude","Buffer")]))
  SS[,Buffer:=as.numeric(Buffer)
     ][,Latitude:=as.numeric(Latitude)
       ][,Longitude:=as.numeric(Longitude)]

  pbuf<-Pbuffer(SS,ID,Projected=F)

  X<-lapply(1:length(Files),FUN=function(i){

    # Progress report
    cat('\r                                                                                              ')
    cat('\r',paste0("Processing dataset: ",i,"/",length(Files)))
    flush.console()

    # Read in raster
    RASTER<-rast(Files[i])

    RASTER.CRS<-terra::crs(RASTER,proj=T)

    # Make sure buffers are in same CRS as raster
    if(as.character(pbuf@proj4string) != RASTER.CRS){
      pbuf1<-terra::vect(sp::spTransform(pbuf,CRS(RASTER.CRS)))
    }else{
      pbuf1<-terra::vect(pbuf)
    }

    A<-gsub(".tif","",tail(unlist(strsplit(Files[i],"/",fixed=T)),1))

    # Progress report
    cat('\r                                                                                              ')
    cat('\r',paste0("Extracting from dataset: ",tail(A,1)," ",i,"/",length(Files)))
    flush.console()

    # extract values from RASTER
    X<-data.table(terra::extract(RASTER,pbuf1))

    colnames(X)[2]<-"Values"

    X<-X[,list(Mean=mean(Values,na.rm=T),
               SD=sd(Values,na.rm=T),
               Median=median(Values,na.rm = T),
               Mode=getmode(Values),N.Cells=.N,
               NA.Cells=length(is.na(Values))),by=ID]

    colnames(X)[2:7]<-paste0(A,".",colnames(X)[2:7])

    X[,-1]


  })

  X<-do.call("cbind",X)
  X<-cbind(SS[,..ID],X)

  if(!is.na(Save_Dir)){
    if(!dir.exists(Save_Dir)){
      dir.create(Save_Dir,recursive = T)
    }

    fwrite(as.data.frame(X),paste0(Save_Dir,Save_Name,".csv"))
  }

  return(X)
}
