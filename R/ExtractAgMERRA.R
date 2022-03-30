#' Extract Climate Data from AgMERRA
#'
#' This function extracts daily climate data from AgMERRA .nc4 files for a dataset containing point locations with a buffer of spatial uncertainty.
#'
#' AgMERRA is the AgMIP climate forcing dataset based on the NASA Modern-Era Retrospective Analysis for Research and Applications (MERRA). AgMERRA
#' corrects to gridded temperature and precipitation, incorporates satellite precipitation, and derives solar radiation from NASA/GEWEX SRB to cover
#' the 1980-2010 period
#'
#' You will need to download all AgMERRA files from \href{https://data.giss.nasa.gov/impacts/agmipcf/agmerra/}{this repository} a directory, then specify this
#' directory as the `AgMERRA_dir` argument.
#'
#' Values are extracted and averaged for bounding boxes created from each unique point location and buffer in the data supplied.
#'
#' @param DATA A data.table or data.frame containing decimal degree point locations as two numeric columns `Latitude` `Longitude`, an numeric field
#' `Buffer` that describes a radius of spatial uncertainty in meters, and a unique id field naming each location whose name is specified using the `ID` parameter.
#' @param ID A character vector of length one containing the column name for a unique id field naming each location in the dataset provided.
#' @param AgMERRA_dir A character vector of length one containing the path to the AgMERRA dataset
#' @param TempDir A character vector of length one containing the path to a temporary directory for AgMERRA processing. Files are saved here until analysis is completed, in case of interruptions.
#' @param Save_Dir A character vector of length one containing the path to the directory where the extracted AgMERRA data is to be saved. Set to NA if you do not want to save the returned dataset.
#' @param cores A numeric vector of length one specifying the number of logical cores to use for parallel processing (do not set to 0 or 1).  Set to NA to disable parallel processing. Default = 4.
#' @param Years A numeric vector of length two specifying the starting and end years for which AgMERRA should be extracted. Default = `c(1980,2010)`.
#' @param M.ORIGIN A character vector of length one in the form dddd-mm-dd specifying a date of origin to which dates are mapped to as the output `DayCount` field. Default value is `1900-01-01`
#' @return A data.table of daily climate data including the fields:
#' * `Year` = Year of observation (integer)
#' * `Day`= Julian day of observation (integer)
#' * `Temp.Mean` = mean temperature - C (numeric)
#' * `Temp.Min` = minimum temperature - C (numeric)
#' * `Temp.Max` = maximum temperature - C (numeric)
#' * `Solar.Rad` = short wave solar radiation - MJ/m2/day (numeric)
#' * `WindSpeed` = windspeed at 2 m above ground level -  m/s (numeric)
#' * `RH.Max` = relative humidity at time of max temp - % (numeric)
#' * `Rain` = precipitation - mm (Numeric)
#' * `Date` = date of observation (Date)
#' * `DayCount` = days since date specified in `M.ORIGIN` parameter
#' @export
#' @importFrom miceadds load.Rdata2
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom parallel makeCluster clusterEvalQ stopCluster clusterExport
#' @importFrom doSNOW registerDoSNOW
#' @import data.table
ExtractAgMERRA<-function(DATA,
                         ID,
                         AgMERRA_dir,
                         TempDir = paste0(getwd(),"/Temp"),
                         Save_Dir = paste0(getwd(),"/AgMERRA"),
                         cores=4,
                         Years = c(1980,2010),
                         M.ORIGIN = "1900-01-01"){

  # SPEED COULD BE IMPROVED BY CONVERTING .NC FILES TO ARRAYS SAVED AS .RDATA OBJECTS (SEE ExtractCHIRPS). ALSO ERROR CHECKING OF NON-MATCHES.SWAP FOR LOOPS FOR LAPPLY


  if(!is.na(TempDir) & substr(TempDir,nchar(TempDir),nchar(TempDir))!="/"){
    TempDir<-paste0(TempDir,"/")
  }

  if(!is.na(Save_Dir) & substr(Save_Dir,nchar(Save_Dir),nchar(Save_Dir))!="/"){
    Save_Dir<-paste0(Save_Dir,"/")
  }

  if(!is.na(AgMERRA_dir) & substr(AgMERRA_dir,nchar(AgMERRA_dir),nchar(AgMERRA_dir))!="/"){
    AgMERRA_dir<-paste0(AgMERRA_dir,"/")
  }

  DATA<-as.data.frame(DATA[!(is.na(DATA$Latitude)|is.na(DATA$Longitude)|is.na(DATA$Buffer))])
  # Create data.frame of unique locations
  SS<-unique(DATA[,c("Latitude","Longitude","Buffer",ID)])

  # Check against existing data to see what new sites need to be extracted or old sites removed
  if(file.exists(paste0(Save_Dir,"AgMERRA.RData"))){

    cat('\r                                                                                                                                          ')
    cat('\r',paste("Cross-referencing against existing extracted data"))
    flush.console()

    X<-miceadds::load.Rdata2(file="AgMERRA.RData",path=Save_Dir)
    YStart<-Years[1]
    YEnd<-Years[2]
    # Check if new years have been added to YStart:YEnd compared to existing dataset
    New.Years<-unique(X$Year)[!YStart:YEnd %in% unique(X$Year)]

    # Check if new sites have been added to Meta.Table that are not in the existing dataset using the ID column
    XSites<-unlist(unique(X[,ID]))
    New.Sites<-SS[,ID][!SS[,ID] %in% XSites]

    Removed.Sites<-XSites[!XSites %in% SS[,ID]]
    X<-X[!unlist(X[,ID]) %in% Removed.Sites,]

    if(length(New.Years)==0 & length(New.Sites)>0){
      # Keep new sites only
      SS<-SS[SS[,ID] %in% New.Sites,]
      DATA<-DATA[DATA[,ID] %in% New.Sites,]
    }else{
      if(length(New.Years)>0 & length(New.Sites)==0){
        YStart<-min(New.Years)
        YEnd<-max(New.Years)
      }}

  }else{
    X<-NULL
  }

  # Buffer Sites
  pbufX<-Pbuffer(Data=DATA,ID,Projected=F)
  # Get bounding boxes
  pbufX<-lapply(pbufX@polygons,bbox)

  # Set temporary save directory for AgMERRA processing
  temp_dir<-paste0(TempDir,"AgMERRA/",Sys.Date(),"/")
  if (!dir.exists(temp_dir)){dir.create(temp_dir,recursive = T)}

  # List Files
  AgMERRA_list<-list.files(path = AgMERRA_dir,".nc4",recursive = T)

  List.Year<-substr(AgMERRA_list,9,12)
  List.Element<-substr(AgMERRA_list,14,nchar(AgMERRA_list)-4)
  Elements<-unique(List.Element)

  fname<-paste0(AgMERRA_dir,"AgMERRA_",1980,"_",Elements[1],".nc4")

  # Match site XY buffers to cells in an AgMERRA ncdf slice
  min_lon <- 1
  min_lon_c <- -1
  min_lat <- 1
  min_lat_c <- -1

  find_open<-ncdf4::nc_open(fname)
  find_lat <- ncdf4::ncvar_get(find_open, "latitude", start = c(min_lat), count = c(min_lat_c))
  find_lon <- ncdf4::ncvar_get(find_open, "longitude", start = c(min_lon), count = c(min_lon_c))
  ncdf4::nc_close(find_open)

  p.xmin<-0
  p.xmax<-0
  p.ymin<-0
  p.ymax<-0

  for(ib2 in 1:length(pbufX)){
    p.xmin[ib2] <- which(abs(find_lon - pbufX[[ib2]][1,1]) == min(abs(find_lon - pbufX[[ib2]][1,1])))
    p.xmax[ib2] <- which(abs(find_lon - pbufX[[ib2]][1,2]) == min(abs(find_lon - pbufX[[ib2]][1,2])))
    p.ymin[ib2] <- which(abs(find_lat - pbufX[[ib2]][2,1]) == min(abs(find_lat - pbufX[[ib2]][2,1])))
    p.ymax[ib2] <- which(abs(find_lat - pbufX[[ib2]][2,2]) == min(abs(find_lat - pbufX[[ib2]][2,2])))
  }

  # Remove non-matches from SS (often islands in other datasets)
  N<-which(is.na(p.xmin)|is.na(p.ymin))

  if(length(N)>0){
    p.xmin<-p.xmin[-N]
    p.xmax<-p.xmax[-N]
    p.ymin<-p.ymin[-N]
    p.ymax<-p.ymax[-N]

    fwrite(SS[N,],paste0(Save_Dir,"AgMERRA Site Errors.csv"))

    SS<-SS[-N,]
  }

  # Set date origin for AgMERRA time dimension
  ORIGIN<-"1980-01-01"

  Data.SYear<-Years[1]
  Data.EYear<-Years[2]

  if(!is.na(cores)){

    # Display progress report
    cat('\r                                                                                                                              ')
    cat('\r',"Parallel processing data")
    flush.console()

    # Create Parallel Clusters
    cl<-makeCluster(cores)
    clusterEvalQ(cl, library(ncdf4))
    doSNOW::registerDoSNOW(cl)

    # Data extraction loop
    foreach (m = Data.SYear:Data.EYear) %dopar% { # YEAR LOOP
      # Filter out irrelevant sites, given the year
      if(file.exists(paste0(temp_dir,"AgMERRA ",m,".RData"))){}else{


        for (k in Elements){ # ELEMENTS LOOP
          # Load .nc4 file for Element*Year
          fname<-paste0(AgMERRA_dir,"AgMERRA_",m,"_",k,".nc4")
          fid1<-ncdf4::nc_open(fname)
          n<-1
          for (l in 1:if((((m %% 4 == 0) & (m %% 100 != 0)) | (m %% 400 == 0))){366}else{365}){  # DAY LOOP
            # Initialize start and count to read one timestep of the variable.
            start <- c(1,1,l)     # set timestep to DAYCOUNT(l)
            count <- c(1440,720,1) # read one timestep for entire spatial extent of datas
            #Extract values from nc4 file for specific day and area, average those values for the area, ignoring any NAs
            VALUES<-as.matrix(ncdf4::ncvar_get(fid1, k, start=start, count=count))
            for(j in 1:nrow(SS)){ # LOCATION LOOP
              VAL<-mean(VALUES[p.xmin[j]:p.xmax[j],p.ymin[j]:p.ymax[j]],na.rm=T)

              if(n==1){
                if(k=="tavg"){
                  Temp.Mean   <- VAL
                  # Update Site & Date holders
                  LAT<-SS$Latitude[j]
                  LON<-SS$Longitude[j]
                  BUF<-SS$Buffer[j]
                  SITE<-SS[j,ID]
                  YEAR<-m
                  DAY<-l
                }
                if(k=="tmin"){Temp.Min    <- VAL}
                if(k=="tmax"){Temp.Max    <- VAL}
                if(k=="srad"){Solar.Rad   <- VAL}
                if(k=="wndspd"){WindSpeed <- VAL}
                if(k=="rhstmax"){RH.Max   <- VAL}
                if(k=="prate"){Rainfall   <- VAL}
              }else{

                if(k=="tavg"){
                  Temp.Mean   <- c(Temp.Mean,VAL)
                  LAT<-c(LAT,as.character(SS$Latitude[j]))
                  LON<-c(LON,as.character(SS$Longitude[j]))
                  SITE<-c(SITE,SS[j,ID])
                  BUF<-c(BUF,SS$Buffer[j])
                  YEAR<-c(YEAR,m)
                  DAY<-c(DAY,l)}
                if(k=="tmin"){Temp.Min    <- c(Temp.Min,VAL)}
                if(k=="tmax"){Temp.Max   <- c(Temp.Max,VAL)}
                if(k=="srad"){Solar.Rad   <- c(Solar.Rad,VAL)}
                if(k=="wndspd"){WindSpeed <- c(WindSpeed,VAL)}
                if(k=="rhstmax"){RH.Max  <- c(RH.Max,VAL)}
                if(k=="prate"){Rainfall  <- c(Rainfall,VAL)}
              }
              n<-n+1 # Update  counter
            } #LOCATIONS
          } # DAYS
          nc_close(fid1)
        } # ELEMENTS
        # Join vectors and save:
        FINAL<-data.frame(ID=SITE,Latitude=as.numeric(as.character(LAT)),Longitude=as.numeric(as.character(LON)),Buffer=BUF,Year=YEAR,Day=DAY,Temp.Mean=Temp.Mean,Temp.Min=Temp.Min,
                          Temp.Max=Temp.Max, Solar.Rad=Solar.Rad,WindSpeed=WindSpeed,RH.Max=RH.Max,Rain=Rainfall)
        save(FINAL,file=paste0(temp_dir,"AgMERRA ",m,".RData"))
      }# END LOGICAL LOOP
    } # YEARS
    stopCluster(cl)
  }else{
    for (m in Data.SYear:Data.EYear){ # YEAR LOOP
      # Filter out irrelevant sites, given the year
      if(file.exists(paste0(temp_dir,"AgMERRA ",m,".RData"))){}else{

        for (k in Elements){ # ELEMENTS LOOP

          # Display progress report
          cat('\r                                                                                                                              ')
          cat('\r',paste("Processing Year: ",m,"/",Data.EYear," | Element: ",k))
          flush.console()

          # Load .nc4 file for Element*Year
          fname<-paste0(AgMERRA_dir,"AgMERRA_",m,"_",k,".nc4")
          fid1<-ncdf4::nc_open(fname)
          n<-1
          for (l in 1:if((((m %% 4 == 0) & (m %% 100 != 0)) | (m %% 400 == 0))){366}else{365}){  # DAY LOOP
            # Initialize start and count to read one timestep of the variable.
            start <- c(1,1,l)     # set timestep to DAYCOUNT(l)
            count <- c(1440,720,1) # read one timestep for entire spatial extent of datas
            #Extract values from nc4 file for specific day and area, average those values for the area, ignoring any NAs
            VALUES<-as.matrix(ncdf4::ncvar_get(fid1, k, start=start, count=count))
            for(j in 1:nrow(SS)){ # LOCATION LOOP
              VAL<-mean(VALUES[p.xmin[j]:p.xmax[j],p.ymin[j]:p.ymax[j]],na.rm=T)

              if(n==1){
                if(k=="tavg"){
                  Temp.Mean   <- VAL
                  # Update Site & Date holders
                  LAT<-SS$Latitude[j]
                  LON<-SS$Longitude[j]
                  BUF<-SS$Buffer[j]
                  SITE<-SS[j,ID]
                  YEAR<-m
                  DAY<-l
                }
                if(k=="tmin"){Temp.Min    <- VAL}
                if(k=="tmax"){Temp.Max    <- VAL}
                if(k=="srad"){Solar.Rad   <- VAL}
                if(k=="wndspd"){WindSpeed <- VAL}
                if(k=="rhstmax"){RH.Max   <- VAL}
                if(k=="prate"){Rainfall   <- VAL}
              }else{

                if(k=="tavg"){
                  Temp.Mean   <- c(Temp.Mean,VAL)
                  LAT<-c(LAT,as.character(SS$Latitude[j]))
                  LON<-c(LON,as.character(SS$Longitude[j]))
                  BUF<-c(BUD,SS$Buffer[j])
                  SITE<-c(SITE,SS[j,ID])
                  YEAR<-c(YEAR,m)
                  DAY<-c(DAY,l)}
                if(k=="tmin"){Temp.Min    <- c(Temp.Min,VAL)}
                if(k=="tmax"){Temp.Max   <- c(Temp.Max,VAL)}
                if(k=="srad"){Solar.Rad   <- c(Solar.Rad,VAL)}
                if(k=="wndspd"){WindSpeed <- c(WindSpeed,VAL)}
                if(k=="rhstmax"){RH.Max  <- c(RH.Max,VAL)}
                if(k=="prate"){Rainfall  <- c(Rainfall,VAL)}
              }
              n<-n+1 # Update  counter
            } #LOCATIONS
          } # DAYS
          ncdf4::nc_close(fid1)
        } # ELEMENTS
        # Join vectors and save:
        FINAL<-data.frame(ID=SITE,Latitude=as.numeric(as.character(LAT)),Longitude=as.numeric(as.character(LON)),Buffer=BUF,Year=YEAR,Day=DAY,Temp.Mean=Temp.Mean,Temp.Min=Temp.Min,
                          Temp.Max=Temp.Max, Solar.Rad=Solar.Rad,WindSpeed=WindSpeed,RH.Max=RH.Max,Rain=Rainfall)
        save(FINAL,file=paste0(temp_dir,"AgMERRA ",m,".RData"))
      }# END LOGICAL LOOP
    } # YEARS
  }

  # Read back in files and combine ####

  AgMERRA_list<-list.files(path = temp_dir)

  for (i in 1:length(AgMERRA_list)){
    cat('\r                                                                                                                                          ')
    cat('\r',paste("Step 2: Combining Years - Progress:",i,"/",length(AgMERRA_list)))
    flush.console()
    if (i==1){
      AgMERRA<-miceadds::load.Rdata2(file=AgMERRA_list[i],path=temp_dir)
    }else{
      Z<-miceadds::load.Rdata2(file=AgMERRA_list[i],path=temp_dir)
      AgMERRA<-rbind(AgMERRA,Z)
    }

  }

  unlink(temp_dir,recursive = T)


  colnames(AgMERRA)[which(colnames(AgMERRA)=="ID")]<-ID

  AgMERRA$Date<-as.Date(AgMERRA$Day - 1, origin = paste0(AgMERRA$Year,"-01-01"))
  AgMERRA$DayCount<-floor(unclass(AgMERRA$Date-as.Date(M.ORIGIN)))

  if(!is.null(X)){
    AgMERRA<-rbind(X,AgMERRA)
  }


  if(!is.na(Save_Dir)){
    if(!dir.exists(paste0(Save_Dir))){
      dir.create(Save_Dir, recursive=T)
    }
    save(AgMERRA,file=paste0(Save_Dir,"AgMERRA.RData"))
  }

  gc()

  return(AgMERRA)
}
