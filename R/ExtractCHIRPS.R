#' Extract Climate Data from reformatted CHIRPS 2.0 dataset
#'
#' For a dataset containing point locations with a buffer of spatial uncertainty this function averages daily precipitation information from the CHIRPS 2.0 dataset.
#'
#' To use this function you will first need to download ref{https://www.chc.ucsb.edu/data/chirps}{CHIRPS 2.0} data using the `DownloadCHIRPS` function
#'  and then reformat this data using the `ReformatCHIRPS` function.
#'
#' @param DATA A data.table or data.frame containing decimal degree point locations as two numeric columns `Latitude` `Longitude`, an numeric field
#' `Buffer` that describes a radius of spatial uncertainty in meters, and a unique id field naming each location whose name is specified using the `ID` parameter.
#' @param ID A character vector of length one containing the column name for a unique id field naming each location in the dataset provided.
#' @param CHIRPS_dir A character vector of length one containing the path to the directory where reformatted CHIRPS data is saved. Default value = `/CHIRPS_Refomatted`.
#' @param Save_Dir A character vector of length one containing the path to the directory where the extracted and compiled POWER data is to be saved. Set to NA if you do not want to save the returned dataset.
#' @param YStart A single integer value from 1983 onward indicating the start year for CHIRPS data extraction.
#' @param YEnd A single integer value from 1983 onward indicating the end year for CHIRPS data extraction.
#' @param ROUND A integer value indicating the number of decimal places to round the output `RAIN` field to. Default = `5`
#' @param M.ORIGIN A character vector of length one in the form `dddd-mm-dd` specifying a date of origin to which dates are mapped to as the output
#'  `DayCount` field. Default value is `1900-01-01`
#' @return A data.table of daily precipitation data including the fields:
#' * `RAIN` = precipitation - mm day-1 (numeric)
#' * `R.Cells` = number of non NA cells contained in a locations bounding box
#' * `R.Cells_0` = number of cells with 0 mm precipitation contained in a locations bounding box
#' * `R.Cells_1` = number of cells with >= 1 mm precipitation contained in a locations bounding box
#' * `R.Cells_5` = number of cells with >= 5 mm precipitation contained in a locations bounding box
#' * `DAY` = julian day of observation (integer)
#' * `LON` = longitude (decimal degrees)
#' * `LAT` = latitude (decimal degrees)
#' * `BUF` =  radius of spatial uncertainty by which location is buffer - m
#' * `ID` = identity field as per `ID` parameter specified
#' * `YEAR` = year of observation (integer)
#' * `Date` = date of observation (Date)
#' * `DayCount` = days since date specified in `M.ORIGIN` parameter
#' @export
ExtractCHIRPS<-function(DATA,
                     ID,
                     CHIRPS_dir="/CHIRPS_Refomatted",
                     Save_Dir = "/CHIRPS_Extracted",
                     YStart,
                     YEnd,
                     ROUND=5,
                     M.ORIGIN="1900-01-01"){

  if(!is.na(Save_Dir)){
    Save_Dir<-paste0(Save_Dir,"CHIRPS/")
    if(!dir.exists(paste0(Save_Dir))){
      dir.create(Save_Dir, recursive=T)
    }
  }


  if(substr(CHIRPS_dir,nchar(CHIRPS_dir),nchar(CHIRPS_dir))!="/"){
    CHIRPS_dir<-paste0(CHIRPS_dir,"/")
  }

  if(substr(Save_Dir,nchar(Save_Dir),nchar(Save_Dir))!="/"){
    Save_Dir<-paste0(Save_Dir,"/")
  }

  DATA<-as.data.frame(DATA[!(is.na(DATA$Latitude)|is.na(DATA$Longitude)|is.na(DATA$Buffer)),])

  # Subset sites
  SS<-unique(DATA[,c(ID,"Latitude","Longitude","Buffer")])

  # List CHIRPS files
  FILES<-list.files(path = CHIRPS_dir)
  FILES.RData<-FILES[grep('.RData', FILES)]
  FILES.Years<-as.numeric(gsub(".RData","",FILES.RData))
  FILES.tif<-FILES[grep('.tif', FILES)]

  # READ RAINFALL DATA ####
  #  Make sure site buffers are in the correct CRS
  p_open<-raster(paste0(CHIRPS_dir,FILES.tif[1]))

  #  BOUNDS
  p_lat <-unique(as.matrix(coordinates(p_open))[,"y"])
  p_lon <-unique(as.matrix(coordinates(p_open))[,"x"])

  # Read in exisiting data and subset new data requirements accordingly
  if(file.exists(paste0(Save_Dir,"CHIRPS.RData"))){
    X<-miceadds::load.Rdata2(file="CHIRPS.RData",path=Save_Dir)

    # Check if new years have been added to YStart:YEnd compare to CHIRPS
    New.Years<-unique(X$Year)[!YStart:YEnd %in% unique(X$Year)]

    # Check if new sites have been added to Meta.Table that are not in CHIRPS using the ID column
    XSites<-unlist(unique(X[,..ID]))
    New.Sites<-SS[,ID][!SS[,ID] %in% XSites]

    Removed.Sites<-XSites[!XSites %in% SS[,ID]]
    X<-X[!unlist(X[,..ID]) %in% Removed.Sites,]

    if(length(New.Years)==0 & length(New.Sites)>0){
      # Keep new sites only
      SS[SS[,ID] %in% New.Sites,]
      DATA[DATA[,ID] %in% New.Sites,]
    }else{
      if(length(New.Years)>0 & length(New.Sites)==0){
        YStart<-min(New.Years)
        YEnd<-max(New.Years)
      }else{
        X<-NULL
      }
    }}else{X<-NULL}

  # Buffer Sites
  pbuf<-Pbuffer(DATA,ID,Projected=F,VERBOSE=F)

  p.xmin<-0
  p.xmax<-0
  p.ymin<-0
  p.ymax<-0

  # Calculate array extraction co-ordinates for each site
  for(i in 1:length(pbuf)){
    # Extract and convert lat/long values to dataset format
    if(((pbuf[i]@bbox[1,1]<min(p_lon) & pbuf[i]@bbox[1,2]<min(p_lon))| (pbuf[i]@bbox[1,1]>max(p_lon) & pbuf[i]@bbox[1,2]>max(p_lon)))){
      p.xmin[i] <-NA
      p.xmax[i] <-NA
    }else{
      p.xmin[i] <- which(abs(p_lon - pbuf[i]@bbox[1,1]) == min(abs(p_lon - pbuf[i]@bbox[1,1])))
      p.xmax[i] <- which(abs(p_lon - pbuf[i]@bbox[1,2]) == min(abs(p_lon - pbuf[i]@bbox[1,2])))
    }
    if(((pbuf[i]@bbox[2,1]<min(p_lat) & pbuf[i]@bbox[2,2]<min(p_lat))| (pbuf[i]@bbox[2,1]>max(p_lat) & pbuf[i]@bbox[2,2]>max(p_lat)))){
      p.ymin[i] <-NA
      p.ymax[i] <-NA
    }else{
      p.ymin[i] <- which(abs(p_lat - pbuf[i]@bbox[2,1]) == min(abs(p_lat - pbuf[i]@bbox[2,1])))
      p.ymax[i] <- which(abs(p_lat - pbuf[i]@bbox[2,2]) == min(abs(p_lat - pbuf[i]@bbox[2,2])))
    }
  }

  # Remove non-matches - for some datasets non-matches are often from sites in islands like Cabo Verde or Mauritius
  N<-which(is.na(p.xmin)|is.na(p.ymin))

  if(length(N)>0){
    p.xmin<-p.xmin[-N]
    p.xmax<-p.xmax[-N]
    p.ymin<-p.ymin[-N]
    p.ymax<-p.ymax[-N]

    fwrite(SS[N,],paste0(Save_Dir,"Site Errors.csv"))

    SS<-SS[-N,]
  }

  Y<-lapply(YStart:YEnd,FUN=function(k){
    cat('\r                                                                                                                                          ')
    cat('\r',paste("Step 1: Loading CHIRPS Data | Year: ",k))
    flush.console()

    RAINDATA<-miceadds::load.Rdata2(FILES.RData[match(k,FILES.Years)],path=CHIRPS_dir)

    Z<-rbindlist(lapply(1:nrow(SS),FUN=function(j){

      # Display progress
      cat('\r                                                                                                                                          ')
      cat('\r',paste("Step 2: Extracting Data | Year: ",k," | Site :",j,"/",nrow(SS)))
      flush.console()

      RAIN<-RAINDATA[p.ymin[j]:p.ymax[j],p.xmin[j]:p.xmax[j],1:dim(RAINDATA)[3]]

      if(is.null(dim(RAIN))){
        Y<-data.table(
          RAIN=round(RAIN,ROUND),
          R.Cells=1,
          R.Cells_0=0,
          R.Cells_1=0,
          R.Cells_5=0,
          DAY=1:length(RAIN)
        )
        Y$R.Cells_0[Y$RAIN==0]<-1
        Y$R.Cells_1[Y$RAIN<=1]<-1
        Y$R.Cells_5[Y$RAIN<=5]<-1
      }else{
        if(length(dim(RAIN))==2){
          Y<-data.table(
            RAIN=round(apply(RAIN,2,mean,na.rm=T),ROUND),
            R.Cells=dim(RAIN)[1],
            R.Cells_0=apply(RAIN,2,FUN=function(x){length(which(x==0))}),
            R.Cells_1=apply(RAIN,2,FUN=function(x){length(which(x<=1))}),
            R.Cells_5=apply(RAIN,2,FUN=function(x){length(which(x<=5))}),
            DAY=1:dim(RAIN)[2]
          )
        }else{
          Y<-data.table(
            RAIN=round(apply(RAIN,3,mean,na.rm=T),ROUND),
            R.Cells=dim(RAIN)[1]*dim(RAIN)[2],
            R.Cells_0=apply(RAIN,3,FUN=function(x){length(which(x==0))}),
            R.Cells_1=apply(RAIN,3,FUN=function(x){length(which(x<=1))}),
            R.Cells_5=apply(RAIN,3,FUN=function(x){length(which(x<=5))}),
            DAY=1:dim(RAIN)[3]
          )
        }
      }


      Y$LON<-SS$Longitude[j]
      Y$LAT<-SS$Latitude[j]
      Y$BUF<-SS$Buffer[j]
      Y$SITE<-SS[j,ID]
      Y$YEAR<-k

      Y

    }))

    suppressWarnings(rm(RAINDATA))

    Z
  })

  Y<-rbindlist(Y)

  colnames(Y)[which(colnames(Y)=="SITE")]<-ID

  Y$Date<-as.Date(Y$DAY - 1, origin = paste0(Y$YEAR,"-01-01"))
  Y$DayCount<-as.integer(floor(unclass(Y$Date-as.Date(M.ORIGIN))))

  if(!is.null(X)){
    Y<-cbind(X,Y)
  }

  if(!is.na(Save_Dir)){

    save(Y,file=paste0(Save_Dir,"CHIRPS.RData"))
  }

  return(Y)
}
