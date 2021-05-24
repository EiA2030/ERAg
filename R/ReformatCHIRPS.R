#' Reformat CHIRPS 2.0 dataset
#'
#' This function reformats the CHIRPS data downloaded by the `DownloadCHIRPS` function to be an array object from which data can be quickly extracted.
#'
#' @param CHIRPS_dir A character vector of length one containing the path to the directory where CHIRPS data should save. Default value = `/CHIRPS`.
#' @param Save_dir A single integer value from 1983 onwards indicating the end year for which CHIRPS data should be downloaded. Default value = `2020`.
#' @param Parallel Memory requirements for parallel processing seem rather high best to keep as F, unless you have a large amount of RAM
#' @export
ReformatCHIRPS<-function(CHIRPS_dir=paste0(getwd(),"/CHIRPS"),
                         Save_dir=paste0(getwd(),"/CHIRPS_Reformatted"))
                         {

  # Add Improvement: as per TARCAT function add lat, lon and day as dim names to the R matrices so we don't need to get these by reading in a

  # List CHIRPS files
  FILES<-list.files(path = CHIRPS_dir,recursive = T)
  FILES<-data.frame(File=FILES[grep('.tif.gz', FILES)])
  FILES<-droplevels(FILES)
  FILES$Year<-as.numeric(as.character(substr(FILES$File,18,21)))
  FILES$Month<-as.numeric(as.character(substr(FILES$File,23,24)))
  FILES$Day<-as.numeric(as.character(substr(FILES$File,26,27)))
  FILES$Date<-as.Date(paste0(FILES$Year,"-",FILES$Month,"-",FILES$Day),"%Y-%m-%d")
  FILES$Yday<-yday(FILES$Date)
  FILES$Code<-paste(FILES$Year,FILES$Yday,sep="-")

  for(i in max(FILES$Year):min(FILES$Year)){

    # Display progress
    cat('\r                                                                                                                                          ')
    cat('\r',i)
    flush.console()

    if(!file.exists(paste0(Save_dir,"/",i,".RData"))){
      X<-FILES$File[FILES$Year==i]

        RAIN<-lapply(X,FUN=function(j){
          # Display progress
          cat('\r                                                                                                                                          ')
          cat('\r',j)
          flush.console()

          if(grepl(".gz",j)){
             Z<-as.matrix(raster::raster(R.utils::gunzip(paste0(CHIRPS_dir,"/",j))))
          }else{
             Z<-as.matrix(raster::raster(paste0(CHIRPS_dir,"/",j)))
          }

          Z[Z== -9999]<-NA
          Z
        })


      Y<-array(unlist(RAIN),dim=c(nrow(RAIN[[1]]),ncol=ncol(RAIN[[1]]),length(RAIN)))
      save(Y,file=paste0(Save_dir,"/",i,".RData"))
    }
  }
}
