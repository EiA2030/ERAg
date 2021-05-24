#' Extract Climate Data from NASA POWER agriclimatology dataset
#'
#' For a dataset containing point locations with a buffer of spatial uncertainty this function downloads and summarizes daily climate information from
#' the ref {NASA POWER}{https://power.larc.nasa.gov/} agroclimatology dataset at 0.5&deg resolution.
#'
#' NASA POWER Solar parameters are derived from NASA's GEWEX/SRB release 3.0 archive (July 1, 1983 – Dec. 31, 2007) and
#' NASA’s CERES FLASHFlux project (Jan. 1, 2008 – to within about 7-days of real time). Meteorological parameters are derived from the NASA's GMAO
#' MERRA-2 assimilation model (Jan. 1, 1981 to within a few months of real time) plus GEOS-5.12.4 FP-IT (End of MERRA-2 to within several days of
#' real time).
#'
#' Values are extracted and averaged for bounding boxes created from each unique point location and buffer in the data supplied.
#'
#' Note older POWER Solar.Rad values are occasionally missing, these can be substituted using AgMERRA data and the `ReplaceSRAD` function.
#'
#' @param DATA A data.table or data.frame containing decimal degree point locations as two numeric columns `Latitude` `Longitude`, an numeric field
#' `Buffer` that describes a radius of spatial uncertainty in meters, and a unique id field naming each location whose name is specified using the `ID` parameter.
#' @param ID A character vector of length one containing the column name for a unique id field naming each location in the dataset provided.
#' @param Parameters A character vector of length one contain the names of POWER variable to be downloaded. Excellent documentation for the POWER API using html web query can be found ref{https://power.larc.nasa.gov/docs/v1/}{here} or
#' ref{https://power.larc.nasa.gov/new/files/POWER_Data_v8_methodology.pdf}{here}. Default value = `paste0("ALLSKY_SFC_SW_DWN,", "PRECTOT,", "PS,","QV2M,","RH2M,","T2M,","T2M_MAX,","T2M_MIN,","WS2M")`
#' @param StartDate A numeric vector of length one containing the start date for POWER data extraction in the form `yyyymmdd`. The default value = `19830701` and this is the earliest date for which POWER data is available
#' @param EndDate A numeric vector of length one containing the start date for POWER data extraction in the form `yyyymmdd`. Default value = 20181231.
#' @param Save_Dir A character vector of length one containing the path to the directory where the extracted and compiled POWER data is to be saved. Set to NA if you do not want to save the returned dataset.
#' @param PowerSave A character vector of length one containing the path to a directory where download POWER data is saved.
#' @param DELETE Logical T/F. Delete downloaded site files once all sites are downloaded and the data has been processed and combined (default = FALSE)
#' @param M.ORIGIN A character vector of length one in the form `dddd-mm-dd` specifying a date of origin to which dates are mapped to as the output `DayCount` field. Default value is `1900-01-01`
#' @param MaxBuffer Maximum allowed buffer radius in m. Default and maximum value = `240000`.
#' @return A data.table of daily climate data including the fields:
#' * `Year` = Year of observation (integer)
#' * `Day`= julian day of observation (integer)
#' * `Solar.Rad` = insolation incident on a horizontal surface - MJ/m^2/day (numeric)
#' * `Pressure` = surface pressure - kPa = (numeric)
#' * `Specific.Humid.` = specific humidity at 2 meters - % (numeric)
#' * `Humid` = relative humidity at 2 meters - % (numeric)
#' * `Temp.Mean` = mean temperature - C (numeric)
#' * `Temp.Min` = minimum temperature - C (numeric)
#' * `Temp.Max` = maximum temperature - C (numeric)
#' * `WindSpeed` = wind speed at 2 m -  m/s (numeric)
#' * `Rain` = precipitation - mm day-1 (numeric)
#' * `Date` = date of observation (Date)
#' * `DayCount` = days since date specified in `M.ORIGIN` parameter
#' @export
ExtractPOWER<-function(DATA,
                    ID,
                    Parameters= paste0("ALLSKY_SFC_SW_DWN,", "PRECTOT,", "PS,","QV2M,","RH2M,","T2M,","T2M_MAX,","T2M_MIN,","WS2M"),
                    StartDate=19830701,
                    EndDate=20181231,
                    Save_Dir="POWER/",
                    PowerSave = "POWER Downloads/",
                    DELETE = F,
                    M.ORIGIN = "1900-01-01",
                    MaxBuffer = 240000){


  if(substr(PowerSave,nchar(PowerSave),nchar(PowerSave))!="/"){
    PowerSave<-paste0(PowerSave,"/")
  }

  if(substr(Save_Dir,nchar(Save_Dir),nchar(Save_Dir))!="/"){
    Save_Dir<-paste0(Save_Dir,"/")
  }

  DATA<-as.data.frame(DATA)

  # Create POWER averaging function:
  P.Avg<-function(POWER,SS,i){
    POWER<-POWER[,lapply(.SD,mean),by="YEAR,DOY"]
    POWER$Buffer<-SS$Buffer[i]
    POWER[,ID]<-SS[i,ID]
    POWER$LAT<-SS$Latitude[i]
    POWER$LON<-SS$Longitude[i]
    return(POWER)
  }

  # Create Save Directory
  if(!dir.exists(PowerSave)){
    dir.create(PowerSave, recursive=T)
  }

  # Create data.frame of unique sites and make vectors
  DATA[,ID]<-as.character(DATA[,ID])
  SS<-unique(DATA[!(is.na(DATA$Longitude)|is.na(DATA$Latitude)|is.na(DATA$Buffer)),c("Latitude","Longitude",ID,"Buffer")])
  SS$Buffer[SS$Buffer>MaxBuffer]<-MaxBuffer

  if(file.exists(paste0(Save_Dir,"POWER.RData"))){
    POWER.LIST<-split(POWER, f = POWER[,ID] )
    Sites<-names(POWER.LIST)
    POWER.LIST<-POWER.LIST[match(Sites,SS[,ID])]
    SS<-SS[!SS[,ID] %in% Sites,]
    DATA<-DATA[!DATA[,ID] %in% Sites,]
  }else{
    POWER.LIST<-NULL
  }


  # Generate site buffers
  pbuf<-Pbuffer(DATA=SS,ID=ID,Projected=F)

  # DOWNLOAD AVERAGE NASA POWER DATA FOR BUFFERED POINT LOCATIONS

  BaseURL<-"https://power.larc.nasa.gov/cgi-bin/v1/DataAccess.py?request=execute"
  n<-1
  nn<-1
  # Create ERROR holder
  ERRORS<-as.character("")

  POWER.NEW<-lapply(1:nrow(SS),FUN=function(i){

    # Create save name
    PName<-paste0(PowerSave,"POWER ",SS[i,ID],".csv")

    # Check to see if file has already been downloaded
    if(file.exists(PName)){

      # Display progress report
      cat('\r                                                                                                                                          ')
      cat('\r',paste("Step 1: Loading Site ",i,"/",nrow(SS)))
      flush.console()

      POWER<-fread(PName)
      POWER<-P.Avg(POWER,SS,i)
      POWER

    }else{

      # Display progress report
      cat('\r                                                                                                                                          ')
      cat('\r',paste("Step 1: Attempting Site ",i,"/",nrow(SS)))
      flush.console()

      # Specify bounding box
      lonmax<-round(max(pbuf[i]@bbox[1,]),5)
      lonmin<-round(min(pbuf[i]@bbox[1,]),5)
      latmax<-round(max(pbuf[i]@bbox[2,]),5)
      latmin<-round(min(pbuf[i]@bbox[2,]),5)

      # Concatenate API query to POWER server
      URL<-paste0(
        BaseURL,
        "&identifier=Regional",
        "&parameters=",Parameters,
        "&startDate=",StartDate,
        "&endDate=",EndDate,
        "&userCommunity=AG",
        "&tempAverage=DAILY",
        "&outputList=CSV",
        "&bbox=",latmin,",",lonmin,",",latmax,",",lonmax,  #lower-left latitude,lower-left longitude,upper-right latitude,upper-right longitude (decimal degrees and no spaces between commas)
        "&user=ICRAF2"
      )

      # Query POWER server and read in response - GET method

      YY<-httr::GET(URL)
      URLLine<-httr::content(YY)$`outputs`$`csv`


      if(length(URLLine)>0){
        # Display progress report
        cat('\r                                                                                                                                          ')
        cat('\r',paste("Step 1: Downloading Site ",i,"/",nrow(SS)))
        flush.console()

        #URLLine<-substr(URLLine,11,nchar(URLLine)-1)
        download.file(URLLine, PName, method="libcurl", quiet = FALSE, mode = "w",cacheOK = TRUE)

        POWER<-fread(PName)
        POWER<-P.Avg(POWER,SS,i)
        POWER

      }else{
        print(paste("Download failed: Site ",i))
      }

    }

  })

  # Name list levels
  names(POWER.NEW)<-SS[,ID]

  # Merge new and old lists
  if(!is.null(POWER.LIST)){
    POWER.NEW<-c(POWER.LIST,POWER.NEW)
  }

  # Note errors
  ERRORS<-as.data.frame(names(POWER.NEW)[is.na(POWER.NEW)])

  # Delete download files?
  if(DELETE==T){
    for(i in list.files(PowerSave)){
      unlink(paste0(PowerSave,i))
    }
  }

  POWER<-rbindlist(POWER.NEW[!is.na(POWER.NEW)])
  CN<-match(c("LAT","LON","DOY","ALLSKY_SFC_SW_DWN","YEAR","PRECTOT","PS","RH2M","T2M_MIN","T2M_MAX","T2M","WS2M","QV2M"),colnames(POWER))
  CN<-CN[!is.na(CN)]
  colnames(POWER)[CN]<-c("Latitude","Longitude","Day","SRad","Year","Rain","Pressure","Humid","Temp.Min","Temp.Max","Temp.Mean","WindSpeed","Specific.Humid")

  # Convert back to data.frame format from data.table ####
  POWER<-as.data.frame(POWER)

  colnames(POWER)[colnames(POWER)=="ID"]<-ID
  POWER[,ID]<-as.character(POWER[,ID])

  # Round values for more efficient compression/smaller object size
  POWER[,c("Rain","Humid","SRad","Temp.Mean","Temp.Max","Temp.Min","WindSpeed","Pressure")]<-round(POWER[,c("Rain","Humid","SRad","Temp.Mean","Temp.Max","Temp.Min","WindSpeed","Pressure")],2)


  POWER<-data.table(POWER)

  # Set -99 SRad values to NA
  POWER[SRad<0,SRad:=NA]

  # Display progress report
  cat('\r                                                                                                                                          ')
  cat('\r',"Step 3a: Adding Date and Daycount since M.ORIGIN")
  flush.console()


  POWER[,Date:=as.Date(Day[1] - 1, origin = paste0(Year[1],"-01-01")),by=c("Day","Year")]
  POWER[,DayCount:=as.integer(floor(unclass(Date[1]-as.Date(M.ORIGIN)))),by=Date]


  if(!is.na(Save_Dir)){
    # Display progress report
    cat('\r                                                                                                                                          ')
    cat('\r',"Step 4: Saving Data")
    flush.console()

    Save_Dir<-paste0(Save_Dir,"POWER/")
    if(!dir.exists(Save_Dir)){
      dir.create(Save_Dir, recursive=T)
    }

    if(nrow(ERRORS)>0){
      fwrite(ERRORS,file=paste0(Save_Dir,"POWER ERRORS.csv"))
    }
    save(POWER,file=paste0(Save_Dir,"POWER.RData"))
  }

  return(POWER)
}
