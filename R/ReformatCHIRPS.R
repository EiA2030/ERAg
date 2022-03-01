#' Reformat CHIRPS 2.0 dataset
#'
#' This function reformats the CHIRPS data downloaded by the `DownloadCHIRPS` function to be an array object from which data can be quickly extracted.
#'
#' @param CHIRPS_dir A character vector of length one containing the path to the directory where CHIRPS data should save. Default value = `/CHIRPS`.
#' @param Save_dir A single integer value from 1983 onwards indicating the end year for which CHIRPS data should be downloaded. Default value = `2020`.
#' @param Parallel Memory requirements for parallel processing seem rather high best to keep as F, unless you have a large amount of RAM
#' @export
#' @importFrom terra rast
#' @importFrom raster raster
ReformatCHIRPS<-function(CHIRPS_dir,
                         Save_dir)
{

  # Add Improvement: as per TARCAT function add lat, lon and day as dim names to the R matrices so we don't need to get these by reading in a

  if(!is.na(CHIRPS_dir) & substr(CHIRPS_dir,nchar(CHIRPS_dir),nchar(CHIRPS_dir))!="/"){
    CHIRPS_dir<-paste0(CHIRPS_dir,"/")
  }

  if(!is.na(Save_dir) & substr(Save_dir,nchar(Save_dir),nchar(Save_dir))!="/"){
    Save_dir<-paste0(Save_dir,"/")
  }


  # List CHIRPS files
  FILES<-data.frame(File=list.files(path = CHIRPS_dir,'.tif'))
  FILES<-droplevels(FILES)

  Dates<-unlist(lapply(strsplit(FILES$File,"v2.0."),"[[",2))
  Dates<-unlist(lapply(strsplit(Dates,".tif"),"[[",1))
  Dates<-as.Date(Dates,format = "%Y.%m.%d")

  FILES$Year<-format(Dates,"%Y")
  FILES$Month<-format(Dates,"%m")
  FILES$Day<-format(Dates,"%d")
  FILES$Date<-Dates
  FILES$Yday<-format(Dates,"%j")
  FILES$Code<-paste(FILES$Year,FILES$Yday,sep="-")


  for(i in min(FILES$Year):max(FILES$Year)){

    # Display progress
    cat('\r                                                                                                                                          ')
    cat('\r',i)
    flush.console()

    if(!file.exists(paste0(Save_dir,i,".RData"))){
      X<-FILES$File[FILES$Year==i]

      RAIN<-lapply(X,FUN=function(j){
        # Display progress
        cat('\r                                                                                                                                          ')
        cat('\r',j)
        flush.console()

        if(grepl(".gz",j)){
          Z<-terra::rast(R.utils::gunzip(paste0(CHIRPS_dir,j),remove=F,temporary=T,overwrite=T))
        }else{
          Z<-terra::rast(paste0(CHIRPS_dir,j))
        }

        Z<-matrix(terra::values(Z), dim(Z)[1], dim(Z)[2], byrow=TRUE)



        Z[Z== -9999]<-NA
        Z
      })


      Y<-array(unlist(RAIN),dim=c(nrow(RAIN[[1]]),ncol=ncol(RAIN[[1]]),length(RAIN)))
      save(Y,file=paste0(Save_dir,i,".RData"))
    }
  }
}
