## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load packages, eval=F,echo=T---------------------------------------------
#    if(!require("pacman", character.only = TRUE)){
#      install.packages("pacman",dependencies = T)
#    }
#  
#    required.packages <- c("data.table","doSNOW","ERAg","ERAgON","ncdf4","raster","rgeos","sp","terra")
#    p_load(char=required.packages,install = T,character.only = T)
#  

## ----Subset ERA,echo=T,eval=F-------------------------------------------------
#  ERA.Data<-ERA.Compiled[Buffer<50000]

## ----set M.Origin,echo=T,eval=F-----------------------------------------------
#  M.ORIGIN<-"1900-01-01"

## ----AgMERRA,echo = T, eval = F-----------------------------------------------
#  
#  AgMERRA<-ERAgON::ExtractAgMERRA(DATA=ERA.Data,
#                ID="Site.Key",
#                AgMERRA_dir = "/AgMERRA Downloads",
#                TempDir = "/Temp",
#                Save_Dir = "/AgMERRA",
#                cores=4,
#                Years = c(1980,2010),
#                M.ORIGIN = M.ORIGIN)

## ----download CHIRPS,echo=T,eval=FALSE----------------------------------------
#  ERAgON::DownloadCHIRPs(StartYear=1983,
#                 EndYear=2019,
#                 SaveDir=paste0(getwd(),"/CHIRPS Downloads"))

## ----reformat CHIRPS,echo=T,eval=FALSE----------------------------------------
#  ERAgON::ReformatCHIRPS<-function(CHIRPS_dir="CHIRPS Downloads"),
#                           Save_dir="CHIRPS Reformatted")

## ----extract CHIRPS,echo=T,eval=FALSE-----------------------------------------
#    CHIRPS<-ERAgON::ExtractCHIRPS(
#      Data=Data,
#      ID="Site",
#      CHIRPS_dir = "CHIRPS Reformatted",
#      Save_Dir = "CHIRPS",
#      YStart=1983,
#      YEnd=2020,
#      Round = 2,
#      Origin = "1900-01-01"
#    )

## ----Add Altitude,eval=F------------------------------------------------------
#  ERA.Data<-merge(ERA.Data,ERA_Physical[,list(Site.Key,Altitude.mean)],by="Site.Key",all.x =T)

## ----POWER,echo = T, eval = F-------------------------------------------------
#  POWER<-ERAgON::ExtractPOWER(Data=ERA.Data,
#                      ID="Site",
#                      Parameters=c("ALLSKY_SFC_SW_DWN", "PRECTOT", "PS","QV2M","RH2M","T2M","T2M_MAX","T2M_MIN","WS2M"),
#                      Rename= c("Solar.Rad","Rain","Pressure","Humid","Temp.Min","Temp.Max","Temp.Mean","WindSpeed","Specific.Humid"),
#                      StartDate="1983-07-01",
#                      EndDate="2021-12-31",
#                      Save_Dir="POWER",
#                      PowerSave = "POWER/Downloads",
#                      Delete=F,
#                      MaxBuffer=240000,
#                      AddDate=T,
#                      AddDayCount=T,
#                      Origin = "1900-01-01",
#                      Quiet=T)

## ----sub SRad AgMERRA, echo=T,eval=F------------------------------------------
#        N<-POWER[,which(is.na(SRad))]
#        M<-match(paste(unlist(POWER[N,..ID]),POWER[N,DayCount]),paste(unlist(AgMERRA[,..ID]),AgMERRA[,DayCount]))
#        NN<-which(!is.na(M))
#        POWER[N[NN],SRad:=AgMERRA[M[NN],Solar.Rad]]
#        POWER[N[NN],SRad.Sub:="AgMERRA"]
#        rm(N,NN)

## ----sub SRad nearby, echo=T,eval=F-------------------------------------------
#        N<-POWER[,which(SRad<0|is.na(SRad))]
#        if(length(N)>0){
#        R<-ReplaceSRAD(SRad = POWER[,SRad],Rain = POWER[,Rain],N,SeqLen=3)
#        POWER[N,SRad:=R]
#        POWER[N,SRad.Sub:="Nearby"]
#        rm(N,R)
#  

## ----Add PET, echo =T, eval=F-------------------------------------------------
#      POWER[,ETo:=ERAg::PETcalc(Tmin=Temp.Min,
#                        Tmax=Temp.Max,
#                        SRad=SRad,
#                        Wind=WindSpeed,
#                        Rain=Rain,
#                        Pressure=Pressure,
#                        Humid=Humid,
#                        YearDay=Day,
#                        Latitude=Latitude,
#                        Altitude=Altitude)$ETo]

## ----remove missing years, echo =T, eval=F------------------------------------
#        POWER<-POWER[!Year==POWER[Site.Key==POWER$Site.Key[1],list(N=.N),by=Year][N<365,Year]]

## ----calculate annual, echo =T, eval=F----------------------------------------
#        POWER.Annual<-POWER[,list(Total.Rain=sum(Rain),
#                                  Total.ETo=sum(ETo),
#                                  S.Humid.Mean=mean(Specific.Humid),
#                                  Humid.Mean=mean(Humid),
#                                  Temp.Mean.Mean=mean(Temp.Mean),
#                                  Mean.N.30.Days=sum(Temp.Mean>30),
#                                  Temp.Max.Mean=mean(Temp.Max),
#                                  Temp.Max=max(Temp.Max),
#                                  Max.N.40.Days=sum(Temp.Max>40),
#                                  Temp.Min.Mean=mean(Temp.Min),
#                                  Temp.Min=min(Temp.Min)),by=c("Site.Key","Year")]

## ----calculate LTA, echo =T, eval=F-------------------------------------------
#        POWER.LT<-POWER.Annual[,list(Total.Rain.Mean=mean(Total.Rain),
#                                     Total.ETo.Mean=mean(Total.ETo),
#                                     S.Humid.Mean=mean(S.Humid.Mean),
#                                     Humid.Mean=mean(Humid.Mean),
#                                     Temp.Mean.Mean=mean(Temp.Mean.Mean),
#                                     Mean.N.30.Days=mean(Mean.N.30.Days),
#                                     Temp.Max.Mean=mean(Temp.Max.Mean),
#                                     Temp.Max=mean(Temp.Max),
#                                     Max.N.40.Days=mean(Max.N.40.Days),
#                                     Temp.Min.Mean=mean(Temp.Min.Mean),
#                                     Temp.Min.Mean=mean(Temp.Min),
#                                     Total.Rain.sd=sd(Total.Rain),
#                                     Total.ETo.sd=mean(Total.ETo),
#                                     S.Humid.Mean.sd=sd(S.Humid.Mean),
#                                     Humid.Mean.sd=sd(Humid.Mean),
#                                     Temp.Mean.Mean.sd=sd(Temp.Mean.Mean),
#                                     Mean.N.30.Days.sd=sd(Mean.N.30.Days),
#                                     Temp.Max.Mean.sd=sd(Temp.Max.Mean),
#                                     Temp.Max.sd=sd(Temp.Max),
#                                     Max.N.40.Days.sd=sd(Max.N.40.Days),
#                                     Temp.Min.Mean.sd=sd(Temp.Min.Mean),
#                                     Temp.Min.sd=sd(Temp.Min)
#        ),by=c("Site.Key")]

## ----CS load packages, eval=F,echo=T------------------------------------------
#    if(!require("pacman", character.only = TRUE)){
#      install.packages("pacman",dependencies = T)
#    }
#  
#    required.packages <- c("ggplot2","circular","data.table","doSNOW","zoo","pbapply","ERAg","dismo")
#    p_load(char=required.packages,install = T,character.only = T)

## ----CS create dirs, eval=F,echo=T--------------------------------------------
#  # Create a directory for the climate analysis
#  AnalysisDir<-"ERA Climate Analysis/"
#  if(!dir.exists(AnalysisDir)){
#    dir.create(AnalysisDir)
#  }
#  
#  # Create a directory for strange or incorrect ERA values to investigate
#  ErrorDir<-paste0(AnalysisDir,"Potential Errors/")
#  if(!dir.exists(ErrorDir)){
#    dir.create(ErrorDir)
#  }
#  
#  # Create a directory for crop specific information
#  CROP_dir<-paste0(AnalysisDir,"Crops/")
#  if(!dir.exists(CROP_dir)){
#    dir.create(CROP_dir)
#  }
#  

## ----CS read in ERA, eval=F,echo=T--------------------------------------------
#  ERA<-ERAg::ERA.Compiled

## ----CS download large file, echo=TRUE,eval=F---------------------------------
#  Sys.setenv(GITHUB_TOKEN = "ghp_XXXXXXXXXXXXXXXXXXXXXXXX")
#  
#  LargeDir<-"Large Files/"
#  
#    if(!dir.exists(LargeDir)){
#      dir.create(LargeDir,recursive = T)
#    }
#  
#    for(FILE in c("POWER.RData","AgMERRA.RData")){
#      piggyback::pb_download(file= FILE,
#                  repo = "peetmate/ERAg",
#                  tag = "Beta_v0.0.1",
#                  dest = LargeDir)
#    }
#  
#    POWER<-data.table(load("POWER.RData",path=LargeDir))
#    CHIRPS<-data.table(load("CHIRPS.RData",path=LargeDir))
#  

## ----CS set parameters, eval=F,echo=T-----------------------------------------
#  # Set date origin for daycount field
#  M.ORIGIN<-"1900-01-01"
#  
#  # Set number of cores for parallel processing
#  cores<-max(1, parallel::detectCores() - 1)
#  
#  # Name of field that assigns sites a unique ID (in ERA this is a concatenation of Latitude, Longitude and Buffer)
#  ID<-"Site.Key"
#  

## ----CS prepare ERA 1, eval=F,echo=T------------------------------------------
#  ERA[Season.Start==1 & Season.End==1,Season:="1"
#             ][Season.Start==2 & Season.End==2,Season:="2"
#               ][Season.Start==1 & Season.End==2,Season:="1&2"
#                 ][M.Year=="",M.Year:=NA
#                   ][Season=="",Season:=NA]

## ---- CS ensure date class, eval=F,echo=T-------------------------------------
#  ERA[,Harvest.Start:=as.Date(Harvest.Start,"%d.%m.%Y")
#      ][,Harvest.End:=as.Date(Harvest.End,"%d.%m.%Y")
#        ][,Plant.Start:=as.Date(Plant.Start,"%d.%m.%Y")
#          ][,Plant.End:=as.Date(Plant.End,"%d.%m.%Y")]

## ----CS add altitude, eval=F,echo=T-------------------------------------------
#  Physical<-data.table(ERAgON::ERA_Physical)
#  
#  # Add DEM altitude to ERA
#  ERA[!is.na(Latitude),Altitude.DEM:=Physical[match(unlist(ERA[!is.na(Latitude),..ID]),unlist(Physical[,..ID])) , Altitude.mean]]
#  
#  # Replace missing altitude values with DEM values
#  ERA[,Altitude:=Elevation
#             ][is.na(Altitude)|Altitude=="",Altitude:=Altitude.DEM]

## ----CS code tables, echo=T, eval=F-------------------------------------------
#  # Ensure code tables are data.table format
#  PracticeCodes<-data.table(ERAg::PracticeCodes)
#  OutcomeCodes<-data.table(ERAg::OutcomeCodes)
#  EUCodes<-data.table(ERAg::EUCodes)

## ----CS subset yields, echo=T, eval=F-----------------------------------------
#  
#  Exclude.Product.Subtypes<-c("Fibre & Wood","Tree","Fodders","Nuts")
#  
#  Exclude.Products<-c("Sugar Cane (Cane)", "Sugar Cane (Sugar)", "Coffee", "Gum arabic", "Cassava or Yuca", "Pepper", "Vanilla", "Tea", "Plantains and Cooking Bananas", "Jatropha (oil)", "Palm Oil", "Olives (fruits)" , "Olives (oil)", "Taro or Cocoyam or Arrowroot", "Palm Fruits (Other)", "Turmeric", "Ginger", "Cardamom", "Apples", "Oranges", "Grapes (Wine)", "Peaches and Nectarines", "Jujube", "Jatropha (seed)", "Vegetables (Other)", "Herbs", "Pulses (Other)", "Beans (Other)", "Leafy Greens (Other)", "Bananas (Ripe Sweet)", "Jatropha")
#  
#  ERA.Yields<-ERA[Out.SubInd == "Crop Yield" & # Subset to crop yields
#                    (M.Year.Start == M.Year.End & (Season %in% c(1,2)|is.na(Season))) & # No temporal aggregation
#                    !Product.Subtype %in% Exclude.Product.Subtypes &  # Subset to single season annual products
#                    !Product %in%  Exclude.Products & # Subset to single season annual products
#                    !grepl(" x |-",Product) & # Single products only
#                    Buffer<=25000]

## ----CS add EcoCrop, echo=T, eval=F-------------------------------------------
#  ERA.Yields<-cbind(ERA.Yields,ERAgON::AddEcoCrop(Products = ERA.Yields[,Product]))

## ----CS prepare climate dataset,echo=T,eval=F---------------------------------
#  
#    # Harmonize field names between datasets
#    colnames(CHIRPS)[which(colnames(CHIRPS)=="RAIN")]<-"Rain"
#    colnames(POWER)[which(colnames(POWER)=="SRad")]<-"Solar.Rad"
#  
#    # Add altitude to POWER
#    POWER[,Altitude:=Physical[match(as.factor(unlist(POWER[,..ID])),as.factor(unlist(Physical[,..ID]))),Altitude.mean]]
#  
#    # Remove observations without Altitude and remove the Rain column which will be replaced by CHIRPS data
#    POWER<-POWER[!is.na(Altitude)][,Rain:=NULL]
#  
#    # We will use the dplyr::left_join function to merge the dataset so we convert from data.table to data.frame
#    CHIRPS<-as.data.frame(CHIRPS)
#    POWER<-as.data.frame(POWER)
#  
#    # Combine datasets and convert back to data.table
#    POWER.CHIRPS<-data.table(dplyr::left_join(POWER,CHIRPS[,c("DayCount",ID,"Rain")],by = c(ID,"DayCount")))
#  
#    # Calculate ETo if absent
#    if(POWER.CHIRPS[,is.null(ETo)]){
#    POWER.CHIRPS[,ETo:=ERAgON::PETcalc(Tmin=Temp.Min,
#                              Tmax=Temp.Max,
#                              SRad=Solar.Rad ,
#                              Wind=WindSpeed,
#                              Rain=Rain,
#                              Pressure=Pressure,
#                              Humid=Humid,
#                              YearDay=Day,
#                              Latitude=Latitude,
#                              Altitude=Altitude)[,1]
#    }
#  
#    # Ensure fields are correct classes
#    POWER.CHIRPS[,Site.Key :=as.factor(Site.Key )
#                 ][,Year:=as.factor(Year)
#                   ][,Day:=as.factor(Day)
#                     ][,Altitude:=as.integer(Altitude)]

## ----CS EstPDayData,echo=T,eval=F---------------------------------------------
#  ERA.Yields<-EstPDayData(DATA=ERA.Yields)$DATA
#  
#  # Estimate % of missing data that could be substituted using EstPDayData estimates
#  round(sum(!is.na(ERA.Yields$Data.PS.Date))/sum(is.na(ERA.Yields$Plant.Start))*100,2)
#  

## ----CS EstSLenData,echo=T,eval=F---------------------------------------------
#  ERA.Yields<-ERAg::EstSLenData(DATA=ERA.Yields)
#  
#  # Estimate % of missing data that could be substituted using EstSLenData estimates
#  round(sum(!is.na(ERA.Yields$SLen))/sum(is.na(ERA.Yields$Data.SLen))*100,2)
#  

## ----echo=T,eval=F------------------------------------------------------------
#    # Estimate season length from date allowing 7 days uncertainty in planting and harvest dates
#    A<-ERA.Yields
#    A<-unique(A[!(is.na(Plant.Start)|is.na(Plant.End)|is.na(Harvest.End)|is.na(Harvest.End))
#                ][,PLANT.AVG:=Plant.Start+(Plant.End-Plant.Start)/2
#                  ][,HARVEST.AVG:=Harvest.Start+(Harvest.End-Harvest.Start)/2
#                    ][!(((Plant.End-Plant.Start)>7)|((Harvest.End-Harvest.Start)>7))
#                      ][,SLEN:=round(HARVEST.AVG-PLANT.AVG,0)
#                        ][!SLEN<30,c("Product",..ID,"Code","Variety","M.Year","PLANT.AVG","SLEN","Country")])
#  
#    B<-A[,list(SLEN.mean=mean(SLEN),SLEN.med=median(SLEN),N=.N),by=c("Product")
#         ][,SLEN.mean:=round(as.numeric(SLEN.mean),0)
#           ][,SLEN.med:=round(as.numeric(SLEN.med),0)]
#  
#    B[,SLEN.EcoC:=round(apply(ERA.Yields[match(B$Product,ERA.Yields$Product),c("cycle_min","cycle_max")],1,mean),0)]
#  
#    # Replace EcoCrop values where we have >=5 observation from our data
#    B<-B[N>=5]
#  
#    # Save estimate crop length datasets
#    fwrite(B,paste0(CROP_dir,"Crop Season Lengths Estimated From Data.csv"))
#  
#    # Add estimate season lengths to ERA where season length values are missing
#    N<-match(ERA.Yields$Product,B$Product)
#    N1<-which(!is.na(N))
#    ERA.Yields$cycle_min[N1]<-B$SLEN.med[N[N1]]
#    ERA.Yields$cycle_max[N1]<-B$SLEN.med[N[N1]]
#  

## ----CS EstPDayRain 2, echo=T, eval=F-----------------------------------------
#  ERA.Yields<-ERAg::EstPDayRain(Data=ERA.Yields,
#                      ID=ID,
#                      Rain.Data = CHIRPS, # You could also use POWER.CHIRPS here
#                      Rain.Data.Name = "CHIRPS",
#                      Rain.Field ="Rain",
#                      # DaysBefore: When searching for rainfall events in-between the uncertain planting start/end dates
#                      # supplied in ERA.Yieds extend the planting start date backwards by 2 days
#                      DaysBefore = 2,
#                      #  MultiplyWin: a proportion that changes the size of the difference between plant start and plant
#                      # end, 1 = no change
#                      MultiplyWin = 1,
#                      # Window: add two addition temporal periods beyond the initial temporal window of 14 days
#                      Window = c(14,14),
#                      # Widths: We need to set a threshold for the rainfall amount in mm that triggers planting within each
#                      # window in this case there are 3 windows 1 + the 2 extra windows specified in the `Window` argument
#                      Widths = c(2,3,3),
#                      # Rain.Thresholds: We need to set a threshold for the rainfall amount in mm that triggers planting in each
#                      # window if `Widths[1]=2` and `Rain.Thresholds[1]=30` then 30mm needs to fall over 2 days within the initial
#                      # window of plant start to plant end dates (as modified by DaysBefore and  MultiplyWin arguments) for
#                      # planting to occur. If the threshold is not met then function iteratively goes to `Window[1]`, `Widths[2]` and
#                      # `Rain.Thresholds[2]`.
#                      Rain.Thresholds = c(30,20,15),
#                      # Uncertainty.Min/Uncertainty.Max: refine planting dates with uncertainty of between 7-90 days
#                      Uncertainty.Min = 7,
#                      Uncertainty.Max = 90,
#                      Add.Values = T,
#                      Use.Data.Dates = T
#                      )
#  
#  

## ----CS CalcClimate, echo=T, eval=F-------------------------------------------
#  Climate<-ERAg::CalcClimate(DATA=ERA.Yields,
#                  ID=ID,
#                  CLIMATE=POWER.CHIRPS,
#                  Rain.Data.Name="CHIRPS",
#                  Temp.Data.Name="POWER",
#                  # Rain.Windows: In estimation of planting dates for long-term climate calculation we will look for
#                  # rainfall events in 4 windows: 6 weeks before planting (the first element supplied is always before
#                  # planting set to 0 if you do not want to look before planting), then in three consecutive windows
#                  # of 4, 2 and 2 weeks
#                  Rain.Windows = c(6*7,4*7,2*7,2*7),
#                  # Widths: To estimate planting dates rainfall is summed in a scanning window for each of the windows
#                  # specified in Rain.Windows; we have 4 windows so need to supply 4 values in days to this argument
#                  Widths = c(3,3,2,2),
#                  # Rain.Threshold: We need to set a threshold for the rainfall amount in mm that triggers planting in
#                  # each combination of `Rain.Window` x `Width`, again 4 values. If Rain.Window = 4*7 & Width = 3 and
#                  # Rain.Threshold = 30 then the function looks within a 21 window for cumulative rainfall of 30mm over
#                  # 3 days
#                  Rain.Threshold = c(30,30,20,15),
#                  # Win.Start: When calculating climate from planting.dates and harvest.dates Win.Start adjust the
#                  # beginning date of the calculation window. -3 means the window begins 3 days before planting
#                  Win.Start=-3,
#                  # Max.LT.Avg: Calculations of long-term climate only use years up to and including 2010
#                  Max.LT.Avg=2010,
#                  Do.LT.Avg=T,
#                  Do.BioClim=T,
#                  # Windows: By default this function looks at two calculation windows `Data` and `EcoCrop`. Additional
#                  # windows can specified the `Windows` argument. In the line below the additional window will called
#                  # `Plant.1-30` in the `W.Name` field of output data.tables and it begins 1 day after planting and ends
#                  # 30 days after planting. This window explores the post-planting climate, a critical period for crop
#                  # establishment and success
#                  Windows=data.table(Name="Plant.1-30",Start=1,End=30),
#                  SaveDir=AnalysisDir,
#                  ErrorDir=NA,
#                  ROUND=3)

## ----CS add climate data, echo=F, eval=T--------------------------------------
Climate<-ERAg::Climate.Vignette

## ----CS Params Output, echo=T,eval=T------------------------------------------
Climate[["Parameters"]]

## ----CS Observed Output, echo=T,eval=T----------------------------------------
Climate[["Observed"]][1:3,]

## ----CS Observed PD Years, echo=T,eval=T--------------------------------------
Climate[["LongTerm"]][["LT.PD.Years"]][1:3,]

## ----CS LTAvg PD Years, echo=T,eval=T-----------------------------------------
Climate[["LongTerm"]][["LT.PD.Avg"]][1:3,]

## ----CS LT.Clim.Years, echo=T,eval=T------------------------------------------
Climate[["LongTerm"]][["LT.Clim.Years"]][1:3,]

## ----CS LT.Clim.Years Variables, echo=T,eval=F--------------------------------
#  Climate[["LongTerm"]][["LT.Clim.Years"]][,unique(Variable)]

## ----Bug hack, echo=F,eval=T--------------------------------------------------
c("Dev.Mean","Dev.Med","Annual Value")

## ----CS LT.Clim.Avg, echo=T,eval=T--------------------------------------------
Climate[["LongTerm"]][["LT.Clim.Avg"]][1:3,]

## ----CS BioClim Years, echo=T,eval=T------------------------------------------
Climate[["BioClim"]][["Annual.Estimates"]][1:3,]

