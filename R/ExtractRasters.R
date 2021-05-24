#' Extract data from multiple rasters or zipped rasters in a single operation
#'
#' For a dataset containing point locations with a buffer of spatial uncertainty this function averages raster values for a
#' character vector pointing to the location of raster files or zipped files. Note if a zipped file containing muliple rasters is supplied
#' it is assumed to be a raster stack and the rasters must share the same extents, origin and  resolutions.
#'
#' Zipped files are unzipped to the working directory, we suggest running `unlink(unzip(zip.filename,list=T)$Name)` to remove these files once extraction is complete.
#'
#' @param DATA A data.table or data.frame containing decimal degree point locations as two numeric columns `Latitude` `Longitude`, an numeric field
#' `Buffer` that describes a radius of spatial uncertainty in meters, and a unique id field naming each location whose name is specified using the `ID` parameter.
#' @param ID A character vector of length one containing the column name for a unique id field naming each location in the dataset provided.
#' @param FILES A character vector listing the paths to raster files. Zipped files can be supplied. If a zipped file containing multiple rasters is supplied
#' it is assumed to be a raster stack and the rasters must share the same extents, origin and  resolutions.
#' @param Save_Dir A character vector of length one containing the path to the directory where the extracted data are to be saved. Set to NA if you do not want to save the returned dataset.
#' @param Save_Name A character vector of length one containing a filename for the returned data.table. Data are saved as a '.RData' object.
#' @return A data.table is returned. For each raster specified in the `FILES` argument columns are created using the raster filename with the following
#' suffixes:
#' * `Mean` = average of non-NA cells for each locations bounding box
#' * `SD` = standard deviation of non-NA cells for each locations bounding box
#' * `Median` = median of non-NA cells for each locations bounding box
#' * `Mode` = mode of non-NA cells for each locations bounding box
#' * `N.Cells` = total number of cells contained each locations bounding box
#' * `NA.Cells` = number of NA cells contained each locations bounding box
#' @export
ExtractRasters<-function(DATA,
                        ID,
                        FILES,
                        Save_Dir,
                        Save_Name){

# Make Mode function
getmode <- function(v) {
  v<-v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

DATA<-as.data.frame(DATA)
SS<-unique(DATA[!(is.na(DATA$Latitude)|is.na(DATA$Latitude)|is.na(DATA$Buffer)),c(ID,"Latitude","Longitude","Buffer")])

pbuf<-Pbuffer(SS,ID,Projected=F)

X<-lapply(1:length(FILES),FUN=function(i){

  # Progress report
  cat('\r                                                                                              ')
  cat('\r',paste0("Processing dataset: ",i,"/",length(FILES)))
  flush.console()

  # Read in raster
  if(grepl(".zip",FILES[i])){

    # Progress report
    cat('\r                                                                                              ')
    cat('\r',paste0("Unzipping dataset: ",i,"/",length(FILES)))
    flush.console()

    RASTER<-rast(unzip(FILES[i]))

    A<-unlist(strsplit(FILES[i],"[/]"))
    A<-gsub(".zip","",A[length(A)])
    ZIP<-T

  }else{

    A<-unlist(strsplit(FILES[i],"[/]"))
    A<-gsub(".tif","",A[length(A)])
    A<-gsub("--SSA","",A)
    RASTER<-rast(FILES[i])
    ZIP<-F
  }

  RASTER.CRS<-crs(RASTER,proj4=T)

  # Make sure buffers are in same CRS as raster
  if(as.character(pbuf@proj4string) != RASTER.CRS){
    pbuf1<-vect(spTransform(pbuf,CRS(RASTER.CRS)))
  }else{
    pbuf1<-vect(pbuf)
  }

  # Progress report
  cat('\r                                                                                              ')
  cat('\r',paste0("Extracting from dataset: ",A," ",i,"/",length(FILES)))
  flush.console()

  # extract values from RASTER
  X<-data.table(extract(RASTER,pbuf1))

  if(!ZIP){
    colnames(X)[2]<-"Values"

    if(is.factor(X[,2])){
      X<-X[,list(Mode=getmode(Values),N.Cells=.N,NA.Cells=length(is.na(Values))),by=ID]
      colnames(X)[2:4]<-paste0(A,".",colnames(X)[2:4])
    }else{
      X<-X[,list(Mean=mean(Values,na.rm=T),SD=sd(Values,na.rm=T),Median=median(Values,na.rm = T),Mode=getmode(Values),N.Cells=.N,NA.Cells=length(is.na(Values))),by=ID]
      colnames(X)[2:7]<-paste0(A,".",colnames(X)[2:7])
    }
    X[,-1]
  }else{

    X<-do.call("cbind",lapply(2:ncol(X),FUN=function(j){
      NAME<-colnames(X)[j]
      j<-c(1,j)
      Y<-X[,..j]
      colnames(Y)[2]<-"Values"

      if(is.factor(Y[,Values])){
        Y<-Y[,list(Mode=getmode(Values),N.Cells=.N,NA.Cells=length(is.na(Values))),by=ID]
        colnames(Y)[2:4]<-paste0(NAME,".",colnames(Y)[2:4])
      }else{
        Y<-Y[,list(Mean=mean(Values,na.rm=T),SD=sd(Values,na.rm=T),Median=median(Values,na.rm = T),Mode=getmode(Values),N.Cells=.N,NA.Cells=length(is.na(Values))),by=ID]
        colnames(Y)[2:7]<-paste0(NAME,".",colnames(Y)[2:7])
      }
      Y[,-1]

    }))

    X


  }

})

X<-do.call("cbind",X)

X<-cbind(SS[,(ID)],X)

if(!is.na(Save_Dir)){
  if(!dir.exists(Save_Dir)){
    dir.create(Save_Dir,recursive = T)
  }

  fwrite(as.data.frame(X),paste0(Save_Dir,Save_Name,".csv"))
}

return(X)
}
