#' Download CHIRPS 2.0 dataset
#'
#' This function download the CHIRPS 2.0 dataset at 0.05 degree resolution from the repository https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_daily/tifs/p05/1981/ to
#' a local directory specied by the `SaveDir` argument.
#'
#' @param StartYear A single integer value from 1983 onward indicating the start year for data download. Default value = `1981`.
#' @param EndYear A single integer value from 1983 onward indicating the end year for data download. Default value = `2021`.
#' @param EndDay A single integer value for the last day, in julian format, of available data for `EndYear`. Default value = `365`.
#' @param SaveDir A character vector of length one containing the path to the directory where CHIRPS data should save. Default value = `/CHIRPS`.
#' @param quiet logical `T/F`, if `T` progress bar is suppressed
#' @export
DownloadCHIRPS<-function(StartYear=1980,EndYear=2021,EndDay=365,SaveDir,quiet){

  URLmaster<-"https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_daily/tifs/p05/"


  if(!dir.exists(SaveDir)){
    dir.create(SaveDir,recursive=T)
  }


  for(YEAR in StartYear:EndYear){ # MinYear:MaxYear (Min = 1983 Max = Present)

    if(YEAR==EndYear){
      ENDDAY<-EndDay
    }else{
      ENDDAY<-as.numeric(format(as.Date(paste0(YEAR,"-12-31")),"%j"))
    }

    for(DAY in 1:ENDDAY){


      if(quiet){
        # Display progress
        cat('\r                                                                                                                                          ')
        cat('\r',paste0("Downloading file: ",DAY,"/",YEAR))
        flush.console()
      }


      DATE<-as.Date(paste0(YEAR,"-",DAY),format="%Y-%j")

      DAY<-format(DATE,"%d")
      MONTH<-format(DATE,"%m")

      FILE<-paste0("chirps-v2.0.",YEAR,".",MONTH,".",DAY,".tif.gz")

      URL<-paste0(URLmaster,YEAR,"/",FILE)
      destfile<-paste0(SaveDir,"/",FILE)
      if(!file.exists(destfile)){
        download.file(URL, destfile,quiet=quiet)
      }
    }
  }
}
