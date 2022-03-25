#' Extract Climate Data from NASA POWER agriclimatology dataset
#'
#' For a dataset containing point locations with a buffer of spatial uncertainty this function downloads and summarizes daily climate information from
#' the  \href{NASA POWER}{https://power.larc.nasa.gov/} agroclimatology dataset at 0.5&deg resolution.
#'
#' NASA POWER Solar parameters are derived from NASA's GEWEX/SRB release 3.0 archive (July 1, 1983 – Dec. 31, 2007) and
#' NASA’s CERES FLASHFlux project (Jan. 1, 2008 – to within about 7-days of real time). Meteorological parameters are derived from the NASA's GMAO
#' MERRA-2 assimilation model (Jan. 1, 1981 to within a few months of real time) plus GEOS-5.12.4 FP-IT (End of MERRA-2 to within several days of
#' real time).
#'
#' Values are extracted and averaged for bounding boxes created from each unique point location and buffer in the data supplied.
#'
#' Parameter descriptions are \href{here}{https://power.larc.nasa.gov/#resources}, details on API query formulation are \href{here}{https://power.larc.nasa.gov/docs/services/api/temporal/daily/} or
#' \href{here}{https://power.larc.nasa.gov/api/pages/?urls.primaryName=Daily}
#'
#' Note older POWER Solar.Rad values are occasionally missing, these can be substituted using AgMERRA data and the `ReplaceSRAD` function.
#'
#' @param Data A data.table or data.frame containing numeric fields for  `Latitude` and `Longitude` (decimal degrees), `Altitude` (m), and `Buffer`  (m). Buffer describes a radius of spatial uncertainty.
#' A character field naming each location is also required, the name of this column must be specified using the `ID` parameter.
#' @param ID A character vector of length one containing the column name for location name field in `Data`.
#' @param Parameters A character vector containing the names of POWER variables to be downloaded.
#' @param Rename A character vector of new variable matching the length of `Parameters`. Set to `NA` to retain orginal parameter names.
#' @param StartDate A numeric vector of length one containing the start date for POWER data extraction in the form `yyyy-mm-dd`. Default value = `1983-07-01` and this is the earliest date for which POWER data is available
#' @param EndDate A numeric vector of length one containing the start date for POWER data extraction in the form `yyyy-mm-dd`.
#' @param Save_Dir A character vector of length one containing the path to the directory where the extracted and compiled POWER data is to be saved. Set to NA if you do not want to save the returned dataset.
#' @param PowerSave A character vector of length one containing the path to a directory where download POWER data is saved.
#' @param Delete Logical `T/F`. Delete downloaded site files once all sites are downloaded and the data has been processed and combined (default = FALSE)
#' @param Origin A character vector of length one in the form `yyyy-mm-dd` specifying a date of origin to which dates are mapped to as the output `DayCount` field. Default value is `1900-01-01`
#' @param MaxBuffer Maximum allowed buffer radius in m. Default and maximum value = `240000`.
#' @param AddDate  Logical `T/F`.If `T` then a date field is added to the output data.table.
#' @param AddDayCount  Logical T/F. If `T` then an integer julian day field is added to the output data.table.
#' @param Round An integer value specifying the number of decimal places to round variable values to.
#' @param Quiet  Logical T/F. Sets the `quiet` argument of the `download.file` function, if `T` a progress bar is displayed.
#' @return A data.table of daily climate data, if default paramters are supplied then output fields are:
#' * `Year` = Year of observation (integer)
#' * `Day`= julian day of observation (integer)
#' * `(ID)` = location name (character)
#' * `Altitude` = altitude of site - m (numeric)
#' * `Solar.Rad` = insolation incident on a horizontal surface - MJ/m^2/day (numeric)
#' * `Pressure` = surface pressure - kPa = (numeric)
#' * `Specific.Humid.` = specific humidity at 2 meters - % (numeric)
#' * `Humid` = relative humidity at 2 meters - % (numeric)
#' * `Temp.Mean` = mean temperature - C (numeric)
#' * `Temp.Min` = minimum temperature - C (numeric)
#' * `Temp.Max` = maximum temperature - C (numeric)
#' * `WindSpeed` = wind speed at 2 m -  m/s (numeric)
#' * `Pressure.Adjusted`= surface pressure adjusted to  - kPa = (numeric)
#' * `Rain` = precipitation - mm day-1 (numeric)
#' * `Date` = date of observation (Date)
#' * `DayCount` = days since date specified in `Origin` parameter
#' @export
#' @import data.table
#' @importFrom data.table fread fwrite
#' @importFrom sp bbox
#' @importFrom miceadds load.Rdata2
ExtractPOWER<-function(Data,
                       ID,
                       Parameters= c("ALLSKY_SFC_SW_DWN", "PRECTOT", "PS","QV2M","RH2M","T2M","T2M_MAX","T2M_MIN","WS2M"),
                       Rename= c("Solar.Rad","Rain","Pressure","Humid","Temp.Min","Temp.Max","Temp.Mean","WindSpeed","Specific.Humid"),
                       StartDate="1983-07-01",
                       EndDate="2021-12-31",
                       Save_Dir="POWER",
                       PowerSave = "POWER/Downloads",
                       Delete = F,
                       MaxBuffer = 240000,
                       AddDate=T,
                       AddDayCount=T,
                       Origin = "1900-01-01",
                       Round=2,
                       Quiet=T){


  if(!is.na(PowerSave) & substr(PowerSave,nchar(PowerSave),nchar(PowerSave))!="/"){
    PowerSave<-paste0(PowerSave,"/")
  }

  if(!is.na(Save_Dir) & substr(Save_Dir,nchar(Save_Dir),nchar(Save_Dir))!="/"){
    Save_Dir<-paste0(Save_Dir,"/")
  }

  Data<-as.data.frame(Data)

  # Create POWER averaging function:
  P.Avg<-function(POWER,SS,i){
    POWER<-miceadds::loadRData2(Save_Dir,"POWER.RData")
    POWER<-POWER[,lapply(.SD,mean),by="YEAR,DOY"]
    POWER$Buffer<-SS$Buffer[i]
    POWER[,ID]<-SS[i,ID]
    POWER$LAT<-SS$Latitude[i]
    POWER$LON<-SS$Longitude[i]
    POWER$Altitude<-SS$Altitude[i]
    return(POWER)
  }

  # Create Save Directory
  if(!dir.exists(PowerSave)){
    dir.create(PowerSave, recursive=T)
  }

  # Create data.frame of unique sites and make vectors
  Data[,ID]<-as.character(Data[,ID])
  SS<-unique(Data[!(is.na(Data$Longitude)|is.na(Data$Latitude)|is.na(Data$Buffer)|is.na(Data$Altitude)),c("Latitude","Longitude",ID,"Buffer","Altitude")])
  SS$Buffer[SS$Buffer>MaxBuffer]<-MaxBuffer

  if(file.exists(paste0(Save_Dir,"POWER.RData"))){
    POWER<-miceadds::load.Rdata2(path=Save_Dir,file="POWER.RData")
    POWER.LIST<-split(POWER, f = POWER[,ID] )
    Sites<-names(POWER.LIST)
    POWER.LIST<-POWER.LIST[match(Sites,SS[,ID])]
    SS<-SS[!SS[,ID] %in% Sites,]
    Data<-Data[!Data[,ID] %in% Sites,]
  }else{
    POWER.LIST<-NULL
  }


  # Generate site buffers
  pbuf<-Pbuffer(Data=SS,ID=ID, Projected = F)
  # Get bounding boxes
  pbuf<-lapply(pbuf@polygons,sp::bbox)

  # DOWNLOAD AVERAGE NASA POWER DATA FOR BUFFERED POINT LOCATIONS
  BaseURL<-"https://power.larc.nasa.gov/api/temporal/daily/"
  n<-1
  nn<-1
  # Create ERROR holder
  ERRORS<-as.character("")


  POWER.NEW<-lapply(1:nrow(SS),FUN=function(i){

    # Display progress report
    cat('\r                                                                                                                                          ')
    cat('\r',paste("Step 1: Attempting Site ",i,"/",nrow(SS)))
    flush.console()

    # Specify bounding box
    lonmax<-round(max(pbuf[[i]][1,]),5)
    lonmin<-round(min(pbuf[[i]][1,]),5)
    latmax<-round(max(pbuf[[i]][2,]),5)
    latmin<-round(min(pbuf[[i]][2,]),5)

    # POWER IS 1/2 lat by 5/8 lon grid
    LatVals<-seq(-89.75,89.75,0.5)
    LonVals<-seq(-179.75,179.75,5/8)

    lonmax<-LonVals[which.min(abs(LonVals-lonmax))]
    lonmin<-LonVals[which.min(abs(LonVals-lonmin))]
    latmax<-LatVals[which.min(abs(LatVals-latmax))]
    latmin<-LatVals[which.min(abs(LatVals-latmin))]

    Cells<-expand.grid(lat=unique(c(latmin,latmax)),lon=unique(c(lonmin,lonmax)))


    StartDate<-as.Date("2017-11-01")
    EndDate<-as.Date("2020-12-31")


    POWER<-rbindlist(lapply(1:nrow(Cells),FUN=function(j){

      # Create save name
      PName<-paste0(PowerSave,"POWER ",SS[i,ID],"-",j,".csv")

      # Check to see if file has already been downloaded
      if(file.exists(PName)){

        # Display progress report
        cat('\r                                                                                                                                          ')
        cat('\r',paste("Step 1: Loading Site",i,"Cell",j,"/",nrow(SS),"Sites"))
        flush.console()

        POWER<-fread(PName)
        setnames(POWER,"PRECTOTCORR","PRECTOT")

      }else{

        # Concatenate API query to POWER server
        URL<-paste0(
          BaseURL,
          "point?start=",format(StartDate,"%Y%m%d"),
          "&end=",format(EndDate,"%Y%m%d"),
          "&latitude=",Cells[j,"lat"],
          "&longitude=",Cells[j,"lon"],
          "&community=ag",
          "&parameters=",paste(Parameters,collapse="%2C"),
          "&format=csv",
          "&user=ICRAF2",
          "&header=true&time-standard=lst",
          "&site-elevation=",SS[i,"Altitude"])


        # Query POWER server and read in response - GET method (URL can be substituted for URLLine below as they are indentical, but this method allows errors to be skipped)

        YY<-httr::GET(URL)
        URLLine<-YY$url

        if(length(URLLine)>0){
          # Display progress report
          cat('\r                                                                                                                                          ')
          cat('\r',paste("Step 1: Downloading Site",i,"Cell",j,"/",nrow(SS),"Sites"))
          flush.console()

          download.file(URLLine, PName, method="libcurl", quiet = Quiet, mode = "w",cacheOK = TRUE)

          POWER<-fread(PName)
          setnames(POWER,"PRECTOTCORR","PRECTOT")

          POWER

        }else{
          print(paste("Download failed: Site" ,i,"Cell",j))
        }

      }

    }),use.names=T)

    POWER<-P.Avg(POWER,SS,i)
    POWER[,NCells:=nrow(Cells)]

    POWER



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
  if(Delete==T){
    for(i in list.files(PowerSave)){
      unlink(paste0(PowerSave,i))
    }
  }

  POWER<-rbindlist(POWER.NEW[!is.na(POWER.NEW)])


  # Set -99 SRad values to NA
  POWER[ALLSKY_SFC_SW_DWN<0,ALLSKY_SFC_SW_DWN:=NA]

  # Rename variables
  if(!(is.null(Rename[1])|is.na(Rename[1]))){
    setnames(POWER,
             c("ALLSKY_SFC_SW_DWN","PRECTOT","PS","RH2M","T2M_MIN","T2M_MAX","T2M","WS2M","QV2M"),
             c("SRad","Rain","Pressure","Humid","Temp.Min","Temp.Max","Temp.Mean","WindSpeed","Specific.Humid"))
  }

  # Renmae ID column
  setnames(POWER,
           c("LAT","LON","DOY","YEAR","PSC"),
           c("Latitude","Longitude","Day","Year","Pressure.Adjusted"))


  # Round values for more efficient compression/smaller object size
  cols<-c("Rain","Humid","SRad","Temp.Mean","Temp.Max","Temp.Min","WindSpeed","Pressure")
  POWER[,(cols) := round(.SD,Round), .SDcols=cols]


  # Display progress report
  cat('\r                                                                                                                                          ')
  cat('\r',"Step 2: Adding Date and Daycount since Origin")
  flush.console()

  if(AddDate){
    POWER[,Date:=as.Date(Day[1] - 1, origin = paste0(Year[1],"-01-01")),by=c("Day","Year")]
  }

  if(AddDayCount){
    POWER[,DayCount:=as.integer(floor(unclass(Date[1]-as.Date(Origin)))),by=Date]
  }


  if(!is.na(Save_Dir)){
    # Display progress report
    cat('\r                                                                                                                                          ')
    cat('\r',"Step 3: Saving Data")
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

