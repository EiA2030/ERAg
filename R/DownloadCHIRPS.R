#' Download CHIRPS 2.0 dataset
#'
#' This function download the CHIRPS 2.0 dataset at 0.05 degree resolution from the repository https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_daily/tifs/p05/1981/ to
#' a local directory specied by the `SaveDir` argument.
#'
#' @param StartYear A single integer value from 1983 onward indicating the start year for data download. Default value = `1981`.
#' @param EndYear A single integer value from 1983 onward indicating the end year for data download. Default value = `2020`.
#' @param SaveDir A character vector of length one containing the path to the directory where CHIRPS data should save. Default value = `/CHIRPS`.
#' @export
DownloadCHIRPs<-function(StartYear=1983,
                         EndYear=2020,
                         SaveDir=paste0(getwd(),"/CHIRPS")){

  numberOfDays <- function(date) {
    m <- format(date, format="%m")

    while (format(date, format="%m") == m) {
      date <- date + 1
    }

    return(as.integer(format(date - 1, format="%d")))
  }


  if(StartYear<1983){StartYear<-1983}

  for(i in StartYear:EndYear){ # MinYear:MaxYear (Min = 1983 Max = Present)
    SaveDir_i<-paste0(SaveDir,"/",i)

    if(!dir.exists(SaveDir_i)){
      dir.create(SaveDir_i,recursive=T)
    }


    Site<-paste0("https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_daily/tifs/p05/")


    for(j in 1:12){
      MDays<-numberOfDays(as.Date(paste0(i,"-",j,"-01")))
      for(k in 1:MDays){
        kk<-if(k<10){paste0(0,k)}else{k}
        jj<-if(j<10){paste0(0,j)}else{j}

        URL<-paste0(Site,i,"/chirps-v2.0.",i,".",jj,".",kk,".tif.gz")
        destfile<-paste0(SaveDir_i,"/chirps-v2.0.",i,".",jj,".",kk,".tif.gz")
        if(!file.exists(destfile)){
          download.file(URL, destfile)
        }


      }}}
}
