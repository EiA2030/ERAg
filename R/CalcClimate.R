#' Calculate climate statistics
#'
#' *ADD DESCRIPTION*
#'
#' Process:
#' 1) Season Length `SLen` is calculated as difference between mean planting and harvest dates.
#'
#' 2) The `SLen` function is applied to the data which combines accurate temporal planting/harvest data with estimates using the following and sequential logic:
#'  i) where `(Plant.End-Plant.Start)>20`and a planting date derived from the `EstPDayRain` function is available `SLen` is substituted for
#'   `Harvest.End` - rainfall estimate planting date;
#'  ii) where `(Plant.End-Plant.Start<20) & (Harvest.End-Harvest.Start>=20)` `SLen` is substituted for `Harvest.End-(Plant.Start+(Plant.End-Plant.Start)/2)`;
#'  iii) A `P.Date.Merge` field is added to `DATA` as `Plant.Start+(Plant.End-Plant.Start)/2`;
#'  iv) Missing `NA` values of `P.Date.Merge` are substituted with estimates derived from similar nearby observations estimated using the `EstPDayData`
#'   function;
#'  v) A `SLen.Merge`  field is added to `DATA` equivalent to the `SLen` field with missing `NA` values substituted with estimates derived from similar nearby
#'   observations estimated using the `EstSLenData` function;
#'  vi) A `SLen.Ecocrop` field is added to `DATA` as `(cycle_min+cycle_max)/2`;
#'  vii) `DATA` is subset to data.table named `SS` which contains unique values of location, product, observation date and planting/harvest/season length
#'   fields. Some form of planting date and season length estimate must be present;
#'  vii) Where there is a difference of greater than 60% between ecocrop vs data estimates of season length observations are rejected.
#'  *The use of Harvest.End in Steps i & ii is deliberate; when uncertainty is large using mean planting and harvest dates can result in unrealistically
#'  short season lengths that distort measures of seasonal climate, especially precipitation. The use of Harvest.End may overestimate season length overall for highly uncertain
#'  scenarios, but we consider the effect this on seasonal climate measures less severe.*
#'
#' 3) For each row of `SS` the `CLIMATE` dataset is filtered on the `Date` field for two temporal periods based on the date fields `P.Date.Merge` and `SLen.Merge`
#'  or `SLen.Ecocrop`, note one day is added to `P.Date.Merge` so the temporal window begins the day after planting. An additional temporal windows can
#'  be specified using data.table supplied to the `Windows` argument, the data.table requires three fields: 1) `Name`; 2) `Start` and 3) `End`. The
#'  `Start` and `End` fields define the additional temporal period as days after`P.Date.Merge`.For example `Window=data.table(Name="Plant.1-30",Start=1,End=30)`
#'  is a temporal period from planting to 30 days after planting.
#'
#' 4) For each temporal period specified in 3) climate statistics are calculated:
#'  i) **growing degrees days (GDD)** are calculated using the `GDDcalc` function which requires daily `Tmax` and `Tmin` fields from `CLIMATE` and the EcoCrop
#'   optimal and absolute temperate thresholds (fields `Tlow`, `Topt.low`, `Topt.high`, and `Thigh`) added to `DATA` using the `AddEcoCrop` function.
#'   The `GDDcalc` function outputs four fields `GDDlow`, `GDDopt`, `GDDhigh` and `GDDmax` and the values of these are summed for the temporal period;
#'  ii) **precipitation and reference evapotranspiration (ETo)** statistics are estimated using the `RAIN.Calc` function which outputs a number of variables
#'   described in the documentation for this function;
#'  iii) **max and mean temperatures** are averaged with standard deviation (`Tmax.Mean`,`Tmax.sd`,`Tmean.mean` and `Tmean.sd` fields).
#'
#' 5) If `Do.LT.Avg==T` annual and long-term climate statistics an planting dates are calculated  for each combination of location and product using the following steps:
#'  i) The median planting julian day of the year is taken for the location x product combination;
#'  ii) For each year of complete climate data, the median planting date from i) is used as a starting point to estimate planting date from rainfall using the
#'  `Est.Rain` function which takes the arguments `Rain.Windows`, `Widths` and `R.mm` as explained in the arguments for this function;
#'  iii)
#'
#' @param DATA An ERA dataset (e.g. `ERA.Compiled`) processed by the `AddEcoCrop`, `EstPDayData`, `EstSLenData` and `EstPDayRain` functions.
#' @param CLIMATE A daily agroclimatology dataset with fields: 1) `Temp.Mean` = mean daily temperature - C; 2) `Temp.Max` = maximum daily temperature - C;
#' `Temp.Min` = minimum daily temperature - C; 4) `Rain` = daily rainfall - mm; 5) `ETo` = reference evapotranspiration - mm; 6) class `Date` field
#'  named `Date`; and 7) location identity as per `ID` argument and named the same as per `DATA`.
#' @param ID
#' @param Rain.Data.Name The name of the rainfall dataset in `CLIMATE`, this must be the same has the rainfall data used when preparing `DATA` using
#' the `EstPDayRain` function. This can be the same as `Temp.Data.Name`.
#' @param Temp.Data.Name The name of the agroclimatology dataset in `CLIMATE`
#' @param Rain.Windows An integer vector; the width in days of four temporal windows within which rainfall is assessed for potential planting dates. The first
#'  value in the series specifies a period before the planting date, then subsequent values define three consecutive windows after the planting date.
#' @param Widths An integer vector of length equivalent to length(`Window`); the width of the scanning window in days within which rainfall is summed for
#' the corresponding `Rain.Windows` entry.
#' @param R.mm An integer vector of length equivalent to length(`Rain.Windows`); the amount of rainfall that has to fall in the temporal windows considered.
#' @param Do.LT.Avg Logical `T/F;` if `T` long term averages and deviances are calculated.
#' @param Max.LT.Avg Integer of length one only relevant if `Do.LT.Avg==T`; maximum year considered when calculating long-term averages.
#' @param Do.BioClim
#' @param Do.Validation
#' @param Windows A data.table with three columns `Name`, `Start` and `End` which specifies additional temporal periods for calculation of climate statistics.
#' For example `data.table(Name="Plant.1-30",Start=1,End=30)` is a temporal period from the day after planting to 30 days after planting. Set to NA if no additional windows are required.
#' @param SaveDir
#' @param ErrorDir
#' @param ROUND
#' @return The following fields are appended to the input dataset:
#' **`[[Observed]]`** = A `data.table` of seasonal climate statistics for the planting
#' **`[[LongTerm]]`** =
#' *`[[LongTerm]][[LT.PD.Years]]`* =
#' *`[[LongTerm]][[LT.PD.Avg]]`* =
#' *`[[LongTerm]][[LT.Clim.Years]]`* =
#'**`[[LongTerm]][[LT.Clim.Avg]]`* =
#' **`[[Annual.BioClim]]`** =
#' *`[[Annual.BioClim]][[Annual.Estimates]]`* =
#' *`[[Annual.BioClim]][[LT.Averages]]`* =
#' *`[[Annual.BioClim]][[Key]]`* =
#' @export
C.Calc<-function(DATA,
                 CLIMATE,
                 ID,
                 Rain.Data.Name,
                 Temp.Data.Name,
                 Rain.Windows,
                 Widths,
                 R.mm,
                 Do.LT.Avg=T,
                 Max.LT.Avg=2010,
                 Do.BioClim,
                 Do.Validation,
                 Windows,
                 SaveDir,
                 ErrorDir,
                 ROUND){

  Est.Rain<-function(Rain,Date,Widths,R.mm,Rain.Windows){

    Rain<-unlist(Rain)
    Date<-unlist(Date)
    PD.N<-Date[1]

    # Make sure the rainfall dataset is complete
    if(!sum(is.na(Rain))>0){

      R<-which(rollapply(as.zoo(Rain[1:Rain.Windows[1]]),width=Widths[1],sum)>R.mm[1])

      if(length(R)>0){
        PD.N+R[1]
      }else{
        R<-which(rollapply(as.zoo(Rain[(Rain.Windows[1]+1):sum(Rain.Windows[1:2])]),width=Widths[2],sum)>R.mm[2])
        if(length(R)>0){
          PD.N+R[1]
        }else{
          R<-which(rollapply(as.zoo(Rain[(sum(Rain.Windows[1:2])+1):sum(Rain.Windows[1:3])]),width=Widths[3],sum)>R.mm[3])
          if(length(R)>0){
            PD.N+R[1]
          }else{
            R<-which(rollapply(as.zoo(Rain[(sum(Rain.Windows[1:3])+1):sum(Rain.Windows[1:4])]),width=Widths[4],sum)>R.mm[4])
            if(length(R)>0){
              PD.N+R[1]
            }else{
              as.numeric(NA)
            }}}}
    }else{
      as.numeric(NA)
    }
  }

  GDD<-function(Tmax,Tmin,Tlow,Topt.low,Thigh,Topt.high,ROUND){

    # Create a sin curve with 24 x-axis intervals
    SIN<-sin(seq(1.5*pi,3.5*pi,(2*pi)/23))
    # Make a new data frame with Tmax, TMin and 24 columns to record hourly interpolated temperatures
    G<-data.frame(cbind(Tmax=Tmax,Tmin=Tmin,matrix(rep(SIN,each=length(Tmin)),ncol=24,nrow=length(Tmin))))

    FUN<-function(X,Tbase,Topt){
      X[X<Tbase]<-0
      X[X>=Tbase & X<=Topt]<- X[X>=Tbase & X<=Topt]-Tbase
      X[X>Topt]<-Topt-Tbase
      return(X)
    }

    GDDcalc<-function(GDDX,SIN,Tmax,Tmin,Tbase,Topt,ROUND){

      # Interpolate Tmax and Tmin using the sin curve
      G[3:26]<-G[3:26]*(G$Tmax-G$Tmin)/(max(SIN)-min(SIN))
      G[3:26]<-G[3:26]+(G$Tmax-apply(G[3:26], 1, max))

      G[3:26]<-sapply(G[3:26],FUN,Tbase,Topt)

      # Average hourly values for each day and add to climate dataset
      G<-round(apply(G[3:26], 1, mean),2)
      return(G)
    }

    # Calculate low GDD ####
    GDDlow<-GDDcalc(SIN,G,Tmax,Tmin,Tbase=Tlow,Topt=Topt.low,ROUND=2)

    # Calculate optimum GDD ####
    GDDopt<-GDDcalc(SIN,G,Tmax,Tmin,Tbase=Topt.low,Topt=Topt.high,ROUND=2)

    # Calculate GDD above optimum, but less than max ####
    GDDhigh<-GDDcalc(SIN,G,Tmax,Tmin,Tbase=Topt.high,Topt=Thigh,ROUND=2)

    # Calculate GDD above max temp ####
    GDDmax<-GDDcalc(SIN,G,Tmax,Tmin,Tbase=Thigh,Topt=999999,ROUND=2)

    # Combine GDD calculations and return
    return(data.table(GDDlow,GDDopt,GDDhigh,GDDmax))

  }

  RAIN.Calc<-function(Rain,ETo){

    ZDays<-function(Rain,Threshold=0,FUN=max){
      Rain[Rain<Threshold]<-0
      FUN(as.numeric(rle(as.character(Rain))$lengths))
    }

    data.table(
      Rain.Days.L.0=round(sum(Rain==0)/length(Rain),2),
      Rain.Days.L.1=round(sum(Rain<1)/length(Rain),2),
      Rain.Days.L.5=round(sum(Rain<5)/length(Rain),2),
      Rain.Max.RSeq.0=ZDays(Rain,Threshold=0,FUN=max),
      Rain.Max.RSeq.0.1=ZDays(Rain,Threshold=0.1,FUN=max),
      Rain.Max.RSeq.1=ZDays(Rain,Threshold=1,FUN=max),
      Rain.Max.RSeq.5=ZDays(Rain,Threshold=5,FUN=max),
      Rain.N.RSeq.T0.D7=ZDays(Rain,Threshold=0,FUN=function(x)sum(x>7)),
      Rain.N.RSeq.T1.D7=ZDays(Rain,Threshold=1,FUN=function(x)sum(x>7)),
      Rain.N.RSeq.T5.D7=ZDays(Rain,Threshold=5,FUN=function(x)sum(x>7)),
      Rain.N.RSeq.T0.D14=ZDays(Rain,Threshold=0,FUN=function(x)sum(x>14)),
      Rain.N.RSeq.T1.D14=ZDays(Rain,Threshold=1,FUN=function(x)sum(x>14)),
      Rain.N.RSeq.T5.D14=ZDays(Rain,Threshold=5,FUN=function(x)sum(x>14)),
      Rain.N.RSeq.T0.D21=ZDays(Rain,Threshold=0,FUN=function(x)sum(x>21)),
      Rain.N.RSeq.T1.D21=ZDays(Rain,Threshold=1,FUN=function(x)sum(x>21)),
      Rain.N.RSeq.T5.D21=ZDays(Rain,Threshold=5,FUN=function(x)sum(x>21)),
      Rain.sum=sum(Rain),
      ETo.sum=sum(ETo),
      ETo.NA=sum(is.na(ETo)),
      WBalance=sum(Rain)-sum(ETo)
    )[is.na(ETo.sum),WBalance:=as.numeric(NA)]


  }

  C.Med<-function(X,FUN){
    if(length(X)>1){
      N<-round(as.numeric(FUN(circular(as.numeric(format(as.Date(paste0("2000",substr(as.character(X), 5, 10))),"%j"))*360/365*pi/180))*180/pi*365/360),0)
      if(N<=0){
        sprintf("%03d",365+N)
      }else{
        sprintf("%03d",N)
      }
    }else{
      N<-round(as.numeric(FUN(circular(as.numeric(format(as.Date(paste0("2000",substr(as.character(X), 5, 10))),"%j"))*360/365*pi/180))*180/pi*365/360),0)
      sprintf("%03s",format(X,"%j"),N)
    }
  }

  SLen<-function(RName,DATA,ErrorDir){
    RName<-paste0("UnC.",RName,".P.Date")

    DATA$Focus<-DATA[,..RName]

    DATA[PDiff>=20 & !is.na(Focus), SLen:=Harvest.End-UnC.CHIRPS.P.Date]
    DATA[,Focus:=NULL]


    DATA[PDiff<20 & H.Diff>=20, SLen:=Harvest.End-(Plant.Start+(Plant.End-Plant.Start)/2)]

    # Logic for merging data sources for planting date and season length
    DATA[,P.Date.Merge:=Plant.Start+(Plant.End-Plant.Start)/2
    ][is.na(P.Date.Merge) & !is.na(Data.PS.Date),P.Date.Merge:=Data.PS.Date+(Data.PE.Date-Data.PS.Date)/2
    ][,SLen.Merge:=as.numeric(as.character(SLen))
    ][is.na(SLen.Merge),SLen.Merge:=as.numeric(as.character(Data.SLen))
    ][,SLen.EcoCrop:=(cycle_min+cycle_max)/2]

    SS<-unique(DATA[!(is.na(SLen.Merge) & is.na(SLen.EcoCrop)) & !is.na(P.Date.Merge),c(..ID,"Code","Latitude","Longitude","EU","Product","M.Year","M.Year.Code","P.Date.Merge","SLen.Merge","SLen.EcoCrop",
                                                                                        "Plant.Start","Plant.End","Harvest.Start","Harvest.End","SLen","Data.SLen","Data.PS.Date","Data.PE.Date",
                                                                                        "Topt.low", "Topt.high","Tlow", "Thigh")])

    # Create Error Save Directory if  Required
    if(!is.na(ErrorDir)){
      if(!dir.exists(ErrorDir)){
        dir.create(ErrorDir,recursive = T)
      }

      Errors<-unique(SS[(abs(SLen.Merge-SLen.EcoCrop)/SLen.EcoCrop)>0.6 & !is.na(Harvest.Start),])

      if(nrow(Errors)>0){
        fwrite(Errors,paste0(ErrorDir,"Suspected Date Error - Season Length Over 60 Perc Diff to EcoCrop Mean - ",RName,".csv"))
      }
    }

    # Fix below this doesn't work when one SLen value is NA

    SS<-SS[,c(..ID,"Latitude","Longitude","EU","Product","M.Year","M.Year.Code","P.Date.Merge","SLen.Merge","SLen.EcoCrop","Topt.low", "Topt.high","Tlow", "Thigh")]
    SS<-SS[!((abs(SLen.Merge-SLen.EcoCrop)/SLen.EcoCrop)>0.6 & !((is.na( SLen.Merge) & !is.na(SLen.EcoCrop))) | (!is.na( SLen.Merge) & is.na(SLen.EcoCrop))),][!(is.na(SLen.Merge) & is.na(SLen.EcoCrop)),]

    return(SS)
  }

  # Analysis parameter code for cross-reference of saved data to current analysis
  Params<-gsub(" ","",paste(paste0(Rain.Windows,collapse=""),
                            paste0(Widths,collapse=""),
                            paste0(R.mm,collapse=""),
                            Max.LT.Avg,
                            paste(apply(Windows[,2:3],1,paste,collapse=""),collapse = "")))

  if(!is.na(SaveDir)){
    SaveDir1<-paste0(SaveDir,"Analysis/",Params,"/")
    # Create Save Directory as Required
    if(!dir.exists(SaveDir1)){
      dir.create(SaveDir1,recursive = T)
    }
  }

  DATA<-data.table(DATA)

  DATA[,Plant.Start:=if(class(Plant.Start)=="Date"){Plant.Start}else{as.Date(Plant.Start,"%d.%m.%Y")}
  ][,Plant.End:=if(class(Plant.End)=="Date"){Plant.End}else{as.Date(Plant.End,"%d.%m.%Y")}
  ][,Harvest.Start:=if(class(Harvest.Start)=="Date"){Harvest.Start}else{as.Date(Harvest.Start,"%d.%m.%Y")}
  ][,Harvest.End:=if(class(Harvest.End)=="Date"){Harvest.End}else{as.Date(Harvest.Start,"%d.%m.%Y")}]

  # Refine Season Length where uncertainty in the timing of planting or harvest exists
  DATA[,PDiff:=Plant.End-Plant.Start][,H.Diff:=Harvest.End-Harvest.Start]


  # Create unique Site x EU x Date combinations
  SS<-SLen(RName=Rain.Data.Name,DATA,ErrorDir)
  MCode.SS<-apply(SS[,c(..ID,"EU","P.Date.Merge","M.Year")],1,paste,collapse="_")

  # Read in previous saved analysis
  SaveName<-paste0(SaveDir1,"ClimStatsA-",Params,substr(Temp.Data.Name,1,3),substr(Temp.Data.Name,1,3),".RData")

  # Cross-reference to exisiting data
  #Missing<-NULL
  MissingLines<-NULL
  S.existing<-NULL

  # Create vector sites in dataset
  Sites<-as.vector(unlist(unique(SS[,..ID])))

  if(nrow(SS)>0){

    CLIMATE<-CLIMATE[,!c("Latitude","Longitude","Buffer","DayCount")]

    if(CLIMATE[,class(Year)]!="integer"){
      CLIMATE[,Year:=as.integer(as.character(Year))]
    }

    if(Do.BioClim){
      CLIMATE1<-data.table::copy(CLIMATE)
    }

    # Set range of years represented in climate data
    Year.Range<-CLIMATE[,min(Year)]:CLIMATE[,max(Year)]

    # Split rainfall data into a list
    CLIMATE<-split(CLIMATE,by=ID)

    # Remove Locations in Climate list not in SS
    N<-match(unlist(SS[,..ID]),names(CLIMATE))
    CLIMATE<-CLIMATE[N[!is.na(N)]]

    # Determine years for which complete data is available (only to be used for annual calcs., e.g. BIOCLIM)
    Years<-table(CLIMATE[[1]]$Year)
    Years<-names(Years[Years>=365])

    B<-lapply(Sites,FUN=function(Site){

      cat('\r                                                                                                                                          ')
      cat('\r',paste0(Temp.Data.Name," x ",Rain.Data.Name,": Estimating seasonal climate for  site: ",match(Site,Sites),"/",length(Sites)))
      flush.console()

      SS.N<-SS[which(SS[,..ID]==Site)][is.na(M.Year.Code),M.Year.Code:=""]

      Climate<-CLIMATE[[Site]]

      if(is.null(Climate)){
        "No site match in Climate Dataset"
      }else{

        A<-rbindlist(lapply(1:nrow(SS.N),FUN=function(i){

          WINS<-rbind(data.table(
            Name=c("Data","EcoCrop"),
            Start=c(1,1),
            End=c(SS.N[i,SLen.Merge], SS.N[i,SLen.EcoCrop])),
            Windows
          )

          Z<-lapply(1:nrow(WINS),FUN=function(j){
            if(!is.na(WINS[j,End])){
              C<-Climate[Climate$Date>=(SS.N[i,P.Date.Merge]+ WINS[j,Start])& Climate$Date<=(SS.N[i,P.Date.Merge] + WINS[j,End])]
              # Make sure the climate dataset is complete
              if(!(nrow(C)<(WINS[j,End]-WINS[j,Start]) | sum(is.na(C$Rain)>0))){ # Technically should have a +1 on the left, here we are giving -1 day leeway

                C<-c(sapply(GDD(Tmax=C$Temp.Max,Tmin=C$Temp.Min,Tlow=SS.N[i,Tlow],Thigh=SS.N[i,Thigh],Topt.low = SS.N[i,Topt.low],Topt.high = SS.N[i,Topt.high],ROUND=2),sum),
                     unlist(RAIN.Calc(C$Rain,C$ETo)),
                     Tmax.mean=mean(C$Temp.Max),Tmax.sd=sd(C$Temp.Max),Tmean.mean=mean(C$Temp.Mean),Tmean.sd=sd(C$Temp.Mean))

                suppressWarnings(C$EU<-SS.N$EU[i])
                C$PD.Used<-SS.N$P.Date.Merge[i]
                C$W.Start<-WINS$Start[j]
                C$W.End<-WINS$End[j]
                C$W.Name<-WINS$Name[j]
                C$ID<-Site
                C$M.Year<-SS.N$M.Year[i]
                C$Season<-SS.N$M.Year.Code[i]
                as.data.frame(C)

              }else{
              }
            }else{

            }

          })

          rbindlist(Z[unlist(lapply(Z,FUN=function(X){!is.null(X)}))])


        }))

        if(Do.LT.Avg){

          EU.N.S<-unique(SS.N[,c("EU","M.Year.Code")])

          LT.A<-lapply(1:nrow(EU.N.S),FUN=function(i){

            cat('\r                                                                                                                                          ')
            cat('\r',paste0(Temp.Data.Name," x ",Rain.Data.Name,": Estimating Long-term averages for  site: ",match(Site,Sites),"/",length(Sites), " | EU: ",EU.N.S$EU[i]))
            flush.console()

            PDates<-as.Date(paste0(Year.Range,"-",SS.N[EU==EU.N.S$EU[i] & M.Year.Code==EU.N.S$M.Year.Code[i],C.Med(P.Date.Merge,FUN=median)]),format="%Y-%j")

            C<-data.table(Start=which(Climate$Date %in% PDates)-Rain.Windows[1])[,End:=Start+sum(Rain.Windows)][!Start<0][!End>nrow(Climate)]
            C<-Climate[unlist(lapply(1:nrow(C),FUN=function(l){unlist(C[l,1]):unlist(C[l,2])})),][!is.na(Year)]

            # Give sequence codes to years using date
            X<-as.numeric(C$Date)
            X<-c(1,X[2:length(X)]-X[1:(length(X)-1)])
            X.N<-c(which(X>1),length(X))

            # Determine sequences of consecutive days & check they are complete
            Y<-lapply(1:length(X.N),FUN=function(i){
              if(i==1){
                rep(i,length(X[1:(X.N[i]-1)]))
              }else{
                if(i!=length(X.N)){
                  rep(i,length(X[X.N[i-1]:(X.N[i]-1)]))
                }else{
                  rep(i,length(X[(X.N[i-1]):X.N[i]]))
                }
              }
            })

            # Add sequences to climate data
            C$Sequence<-unlist(Y)

            # Remove incomplete sequences by cross referencing the width the window should be
            C<-C[Sequence %in% which(unlist(lapply(Y,length)) == sum(Rain.Windows)+1)]

            # Work out planting date for each sequence
            Annual.Plant<-as.Date(unlist(C[,Est.Rain(Rain,Date,..Widths,..R.mm,..Rain.Windows),by=Sequence][,2]))

            # Record the planting year of each sequence (the planting date used as the starting point to search from, i.e. PDates)
            P.Years<-unlist(C[c(1, cumsum(rle(C$Sequence)$lengths) + 1)+Rain.Windows[1],"Year"][!is.na(Year)])

            if(!sum(is.na(Annual.Plant))>length(Annual.Plant)/1.5){

              N<-!is.na(Annual.Plant)
              N1<-!is.na(Annual.Plant) &  as.integer(format(Annual.Plant,"%Y"))<=Max.LT.Avg

              if(length(N1)>0){

                LT.Mean<-as.numeric(round(mean(circular(as.numeric(format(as.Date(paste0("2000",substr(as.character(Annual.Plant[N1]), 5, 10))),"%j"))*360/365*pi/180))*180/pi*365/360,ROUND))
                LT.Median<-as.numeric(round(median(circular(as.numeric(format(as.Date(paste0("2000",substr(as.character(Annual.Plant[N1]), 5, 10))),"%j"))*360/365*pi/180))*180/pi*365/360,ROUND))

                if(LT.Mean<=0){
                  LT.Mean<-365+LT.Mean
                }

                if(LT.Median<=0){
                  LT.Median<-365+LT.Median
                }

                # Calculate LT.Avg Planting date standard deviation
                LT.SD<-round(sd(circular(as.numeric(format(as.Date(paste0("2000",substr(as.character(Annual.Plant[N1]), 5, 10))),"%j"))*360/365*pi/180))*180/pi*365/360,ROUND)

                # Calculate Annual Deviation of Planting Date From LT.Avg
                Annual.Dev.Mean<-as.numeric(circular (as.numeric(format(Annual.Plant[N],"%j"))*360/365*pi/180)- # Annual Estimate
                                              circular (as.numeric(LT.Mean)*360/365*pi/180)        # Long Term Average
                )*365/360*180/pi

                Annual.Dev.Median<-as.numeric(circular (as.numeric(format(Annual.Plant[N],"%j"))*360/365*pi/180)- # Annual Estimate
                                                circular (as.numeric(LT.Median)*360/365*pi/180)        # Long Term Average
                )*365/360*180/pi

                NYear.LT.Avg<-sum(N1)

                LT.PlantDates<-list(Annual.Estimates=data.table(P.Year=P.Years[N],P.Date=Annual.Plant[N],Dev.Mean=Annual.Dev.Mean,Dev.Med=Annual.Dev.Median,EU=EU.N.S$EU[i],Season=EU.N.S$M.Year.Code[i],ID=Site),
                                    LT.Averages=data.table(Mean=LT.Mean,Median=LT.Median,SD=LT.SD,N=NYear.LT.Avg,EU=EU.N.S$EU[i],Season=EU.N.S$M.Year.Code[i],ID=Site))

                # Now Estimate Temperature and Rainfall LTAvgs using the average season lengths and pre-set windows
                WINS<-rbind(data.table(
                  Name=c("Data","EcoCrop"),
                  Start=c(1,1),
                  End=c(SS.N[i,SLen.Merge][1], SS.N[i,SLen.EcoCrop][1])),
                  Windows
                )[,End:=round(End,0)][,Start:=round(Start,0)]

                WINS<-WINS[!is.na(End),]

                if(nrow(WINS)>0){

                  Annual.Plant<-Annual.Plant[!is.na(Annual.Plant)]

                  LT.Climate<-lapply(1:nrow(WINS),FUN=function(k){

                    A.Harvest<-Annual.Plant+round(WINS[k,End])
                    A.Plant<-Annual.Plant+WINS[k,Start]

                    A.P.H<-A.Plant %in% Climate$Date & A.Harvest %in% Climate$Date
                    A.Harvest<-A.Harvest[A.P.H]
                    A.Plant<-A.Plant[A.P.H]

                    N1<-which(Climate$Date %in% A.Plant)
                    N2<-which(Climate$Date %in% A.Harvest)

                    C<-cbind(which(Climate$Date %in% A.Plant),which(Climate$Date %in% A.Harvest))

                    C<-Climate[unlist(lapply(1:nrow(C),FUN=function(l){C[l,1]:C[l,2]})),]

                    # Check that climate dataset is complete

                    # Give sequence codes to years using date
                    X<-as.numeric(C$Date)
                    X<-c(1,X[2:length(X)]-X[1:(length(X)-1)])
                    X.N<-c(which(X>1),length(X))

                    # Determine sequences of consecutive days & check they are complete
                    Y<-lapply(1:length(X.N),FUN=function(i){
                      if(i==1){
                        rep(i,length(X[1:(X.N[i]-1)]))
                      }else{
                        if(i!=length(X.N)){
                          rep(i,length(X[X.N[i-1]:(X.N[i]-1)]))
                        }else{
                          rep(i,length(X[(X.N[i-1]):X.N[i]]))
                        }
                      }
                    })

                    # Add sequences to climate data
                    C<-C[,Sequence:=unlist(Y)
                         # Remove incomplete sequences by cross referencing to the width the window should be
                    ][Sequence %in% which(unlist(lapply(Y,length)) >= floor(WINS[k,End]))
                    ]

                    C<-C[,P.Year:=rep(unlist(C[c(1, cumsum(rle(C$Sequence)$lengths) + 1),"Year"][!is.na(Year)]),each=WINS[k,End])
                    ][,H.Year:=rep(unlist(C[cumsum(rle(C$Sequence)$lengths),"Year"][!is.na(Year)]),each=WINS[k,End])
                    ][,!"Sequence"]
                    # Record the harvest year of each sequence (the planting date used as the starting point to search from, i.e. PDates)

                    # Calculate climate stats
                    C<-cbind(
                      C[,GDD(Tmax=Temp.Max,Tmin=Temp.Min,Tlow=SS.N[i,mean(Tlow)],Thigh=SS.N[i,mean(Thigh)],Topt.low = SS.N[i,mean(Topt.low)],Topt.high = SS.N[i,mean(Topt.high)],ROUND=2),by=c("P.Year","H.Year")
                      ][,lapply(.SD,sum),.SDcol=3:6,by=c("P.Year","H.Year")],
                      C[,RAIN.Calc(Rain,ETo),by=c("P.Year","H.Year")][,-c(1:2)],
                      C[,list(Tmax.mean=mean(Temp.Max),Tmax.sd=sd(Temp.Max),Tmean.mean=mean(Temp.Mean),Tmean.sd=sd(Temp.Mean)),by=c("P.Year","H.Year")][,-c(1:2)]
                    )

                    LT.Avg<-C[H.Year<=Max.LT.Avg,lapply(.SD,FUN=function(X){c(round(mean(X,na.rm=T),ROUND),
                                                                              round(median(X,na.rm=T),ROUND),
                                                                              round(sd(X,na.rm=T),ROUND),
                                                                              min(X,na.rm=T),
                                                                              max(X,na.rm=T))}),.SDcols=3:ncol(C)][,Variable:=c("Mean","Median","SD","Min","Max")][,N:=nrow(C[H.Year<=Max.LT.Avg,1])]

                    suppressWarnings(Annual.Estimates<-as.data.table(rbind(
                      (C[,!c("H.Year","Variable","P.Year")]-LT.Avg[Variable=="Mean",1:(ncol(LT.Avg)-2)][rep(1,nrow(C)),])[,P.Year:=C[,P.Year]][,H.Year:=C[,H.Year]][,Variable:="Dev.Mean"],
                      (C[,!c("H.Year","Variable","P.Year")]-LT.Avg[Variable=="Median",1:(ncol(LT.Avg)-2)][rep(1,nrow(C)),])[,P.Year:=C[,P.Year]][,H.Year:=C[,H.Year]][,Variable:="Dev.Med"],
                      C[,Variable:="Annual Value"]
                    ))[,!"ETo.NA"])

                    LT.Avg$EU<-EU.N.S$EU[i]
                    LT.Avg$Season<-EU.N.S$M.Year.Code[i]
                    LT.Avg$ID<-Site
                    LT.Avg$W.Start<-WINS[k,Start]
                    LT.Avg$W.End<-WINS[k,End]
                    LT.Avg$W.Name<-WINS[k,Name]

                    Annual.Estimates$EU<-EU.N.S$EU[i]
                    Annual.Estimates$Season<-EU.N.S$M.Year.Code[i]
                    Annual.Estimates$ID<-Site
                    Annual.Estimates$W.Start<-WINS[k,Start]
                    Annual.Estimates$W.End<-WINS[k,End]
                    Annual.Estimates$W.Name<-WINS[k,Name]

                    list(Annual.Estimates=Annual.Estimates,LT.Averages=LT.Avg)

                  })
                  names(LT.Climate)<-WINS[,Name]
                }else{
                  LT.Climate<-NA
                }

                list(LT.PlantDates=LT.PlantDates,LT.Climate=LT.Climate)

              }else{
                NA
              }
            }else{

              if(!dir.exists(paste0(SaveDir1,"Low Rainfall Sites/"))){
                dir.create(paste0(SaveDir1,"Low Rainfall Sites/"))
              }

              fwrite(SS.N,paste0(paste0(SaveDir1,"Low Rainfall Sites/",SS.N[1,Site.Key],".csv")))
              NA
            }

          })

          names(LT.A)<-apply(unique(SS.N[,c("EU","Product","M.Year.Code")]),1,paste,collapse="-")

          A<-list(A,LT.A)
          names(A)<-c("Seasonal Stats","LT Avg & Dev")

        }else{
          A<-list(A)
          names(A)<-"Seasonal Stats"
        }
        A
      }
    })

    names(B)<-Sites

    B<-B[unlist(lapply(B,length)>1)]

    # Bind lists together
    Seasonal.Clim<-rbindlist(lapply(B,"[[","Seasonal Stats"))

    # Add in existing data
    if(!is.null(S.existing)){
      if(nrow(S.existing[["Observed"]])>0){
        Seasonal.Clim<-rbind(S.existing[["Observed"]],Seasonal.Clim)
      }
    }

    MCode<-unique(apply(Seasonal.Clim[,c("ID","EU","PD.Used","M.Year")],1,paste,collapse="_"))
    MCode<-MCode.SS[!MCode.SS %in% MCode]

    if(length(MCode)>0){
      if(!is.null(MissingLines)){
        fwrite(data.table(Missing=unique(c(MissingLines,MCode))),paste0(SaveDir1,"MissingLines-",Params,substr(Temp.Data.Name,1,3),substr(Rain.Data.Name,1,3),".csv"))
      }else{
        fwrite(data.table(Missing=unique(MCode)),paste0(SaveDir1,"MissingLines-",Params,substr(Temp.Data.Name,1,3),substr(Rain.Data.Name,1,3),".csv"))
      }
    }

    if(Do.LT.Avg){

      # Deal with NA values in LT climate data
      NotNA<-lapply(lapply(B,"[[",c("LT Avg & Dev")),FUN=function(X){length(X[[1]])})>1
      B<-B[NotNA]

      # Rbind LT climate data stats together from lists
      for(i in 1:length(B)){
        LT.PD.Years<-rbindlist(lapply(lapply(B[i],"[[",c("LT Avg & Dev")),FUN=function(Y){
          rbindlist(lapply(Y,"[[",c("LT.PlantDates","Annual.Estimates")))
        }))

      }

      LT.PD.Avg<-rbindlist(lapply(lapply(B,"[[",c("LT Avg & Dev")),FUN=function(Y){rbindlist(lapply(Y,"[[",c("LT.PlantDates","LT.Averages")))}))

      LT.Clim.Years<-rbindlist(lapply(lapply(lapply(B,"[[",c("LT Avg & Dev")),FUN=function(Y){lapply(Y,"[[","LT.Climate")}),FUN=function(Y){rbindlist(lapply(Y,FUN=function(Z){
        rbindlist(lapply(Z,"[[","Annual.Estimates"))}))}))

      LT.Clim.Avg<-rbindlist(lapply(lapply(lapply(B,"[[",c("LT Avg & Dev")),FUN=function(Y){lapply(Y,"[[","LT.Climate")}),FUN=function(Y){rbindlist(lapply(Y,FUN=function(Z){
        rbindlist(lapply(Z,"[[","LT.Averages"))}))}))

      if(!is.null(S.existing)){
        if(nrow(S.existing[["LongTerm"]][["LT.PD.Years"]])>0){
          LT.PD.Years<-rbind(S.existing[["LongTerm"]][["LT.PD.Years"]],LT.PD.Years)
          LT.PD.Avg<-rbind(S.existing[["LongTerm"]][["LT.PD.Avg"]],LT.PD.Avg)
          LT.Clim.Years<-rbind(S.existing[["LongTerm"]][["LT.Clim.Years"]],LT.Clim.Years)
          LT.Clim.Avg<-rbind(S.existing[["LongTerm"]][["LT.Clim.Avg"]],LT.Clim.Avg)
        }
      }

      Seasonal<-list(Observed=Seasonal.Clim,LongTerm=list(LT.PD.Years=LT.PD.Years,LT.PD.Avg=LT.PD.Avg,LT.Clim.Years=LT.Clim.Years,LT.Clim.Avg=LT.Clim.Avg))

    }else{
      Seasonal<-list(Observed=Seasonal.Clim)
    }


    if(!is.na(SaveDir1)){
      save(Seasonal,file=SaveName)
    }


  }else{
    Seasonal<-S.existing
  }

  if(Do.BioClim){

    SaveName<-paste0(SaveDir1,"BioClim-",Params,substr(Temp.Data.Name,1,3),substr(Rain.Data.Name,1,3),".RData")

    # Cross-reference to exisiting data
    S.existing1<-NULL
    MCode1<-NULL
    MissingSites<-NULL
    MCode.D<-NULL

    if(file.exists(SaveName)){
      S.existing1<-load.Rdata2(paste0("BioClim-",Params,substr(Temp.Data.Name,1,3),substr(Rain.Data.Name,1,3),".RData"),path=SaveDir1)
      MCode.D<-unlist(unique(DATA[,..ID]))
      MCode.E<-unlist(S.existing1[["Annual.Estimates"]][,"ID"])

      if(file.exists(paste0(SaveDir1,"MissingSites-BIOC-",Params,substr(Temp.Data.Name,1,3),substr(Rain.Data.Name,1,3),".csv"))){
        MissingSites<-unlist(fread(paste0(SaveDir1,"MissingSites-BIOC-",Params,substr(Temp.Data.Name,1,3),substr(Rain.Data.Name,1,3),".csv"),sep=","))
        MCode.D<-MCode.D[!MCode.D %in% MissingSites]
      }

      if(nrow(SS)>0){
        MCode<-unlist(CLIMATE1[,..ID])
        N<-MCode %in% MCode.E
        CLIMATE1<-CLIMATE1[!N]
      }

      S.existing1[["Annual.Estimates"]]<-S.existing1[["Annual.Estimates"]][MCode.E %in% MCode.D]

      MCode.E1<-unlist(S.existing1[["LT.Averages"]][,"ID"])
      S.existing1[["LT.Averages"]]<-S.existing1[["LT.Averages"]][MCode.E1 %in% MCode.D]

      MCode1<-unlist(MCode.D[!MCode.D %in% unique(MCode.E)])

    }

    if(!(!is.null(MCode1) & length(MCode1)==0)){

      if(nrow(SS)==0){

        if(!is.null(MCode1)){
          CCode<-unlist(CLIMATE1[,..ID])
          MCode<-CCode %in% MCode1
        }else{
          CCode<-unlist(CLIMATE1[,..ID])
          MCode<-CCode %in% unlist(unique(DATA[,..ID]))
        }


        # Subset to a single site
        N<-unlist(CLIMATE1[,..ID]) %in% unlist(CLIMATE1[,..ID])[1]
        # Count days in years
        Years<-CLIMATE1[N,"Year"] %>% table

        # Determine years for which complete data is available
        Years<-names(Years[Years>=365])

        CLIMATE1<-CLIMATE1[MCode]

      }

      cat('\r                                                                                                                                          ')
      cat('\r',paste0(Temp.Data.Name," x ",Rain.Data.Name,": Calculating BioClim Annual Variables"))
      flush.console()

      CLIMATE1<-CLIMATE1[Year %in% Years,]
      colnames(CLIMATE1)[colnames(CLIMATE1)==ID]<-"ID"

      BIOV<-CLIMATE1[,round(dismo::biovars(Rain,Temp.Min,Temp.Max),2),by=c("Year","ID")][,Variable:= rep(paste("BIO",1:19),.N/19)]
      BIOV<-dcast(BIOV,Year+ID ~ Variable,value.var="V1")

      # Record data deficient sites
      if(is.null(MCode.D)){
        MCode.D<-unlist(unique(DATA[,..ID]))
      }

      MCode.D<-MCode.D[!MCode.D %in% BIOV$ID]

      if(length(MCode.D)>0){
        if(!is.null(MissingSites)){
          fwrite(data.table(Missing=c(MissingSites,MCode.D)),paste0(SaveDir1,"MissingSites-BIOC-",Params,substr(TName,1,3),substr(RName,1,3),".csv"))
        }else{
          fwrite(data.table(Missing=MCode.D),paste0(SaveDir1,"MissingSites-BIOC-",Params,substr(TName,1,3),substr(RName,1,3),".csv"))
        }
      }

      BIOV.LT<-BIOV[Year<=Max.LT.Avg,pblapply(.SD,FUN=function(X){round(c(mean(X),median(X),sd(X)),2)}),by="ID",.SDcol=3:ncol(BIOV)
      ][,Variable:=rep(c("Mean","Median","SD"),.N/3)][,N:=sum(Years<=Max.LT.Avg)]

      Annual.Estimates<-rbind(
        cbind(BIOV[,1:2],(BIOV[,3:21]-BIOV.LT[Variable=="Mean",][match(BIOV[,ID],BIOV.LT[Variable=="Mean",ID]),-c(1,21,22)])[,Variable:="Dev.Mean"]),
        cbind(BIOV[,1:2],(BIOV[,3:21]-BIOV.LT[Variable=="Median",][match(BIOV[,ID],BIOV.LT[Variable=="Median",ID]),-c(1,21,22)])[,Variable:="Dev.Mean"]),
        BIOV[,Variable:="Annual Value"]
      )

      if(!is.null(S.existing1)){
        if(nrow(S.existing1[["Annual.Estimates"]])>0){
          BIOV.LT<-rbind(S.existing1[["LT.Averages"]],BIOV.LT)
          Annual.Estimates<-rbind(S.existing1[["Annual.Estimates"]],Annual.Estimates)
        }
      }


      BIOV<-list(Annual.Estimates=Annual.Estimates,
                 LT.Averages = BIOV.LT,
                 Key=t(data.table(
                   BIO1 = "Mean annual temperature",
                   BIO2 = "Mean diurnal range (mean of max temp - min temp)",
                   BIO3 = "Isothermality (bio2/bio7) (* 100)",
                   BIO4 = "Temperature seasonality (standard deviation *100)",
                   BIO5 = "Max temperature of warmest month",
                   BIO6 = "Min temperature of coldest month",
                   BIO7 = "Temperature annual range (bio5-bio6)",
                   BIO8 = "Mean temperature of the wettest quarter",
                   BIO9 = "Mean temperature of driest quarter",
                   BIO10 = "Mean temperature of warmest quarter",
                   BIO11 = "Mean temperature of coldest quarter",
                   BIO12 = "Total (annual) precipitation",
                   BIO13 = "Precipitation of wettest month",
                   BIO14 = "Precipitation of driest month",
                   BIO15 = "Precipitation seasonality (coefficient of variation)",
                   BIO16 = "Precipitation of wettest quarter",
                   BIO17 = "Precipitation of driest quarter",
                   BIO18 = "Precipitation of warmest quarter"
                 ))
      )

      if(!is.na(SaveDir1)){
        save(BIOV,file=SaveName)
      }

      Seasonal$Annual.BioClim<-BIOV
    }else{
      Seasonal$Annual.BioClim<-S.existing1
    }
  }



  return(Seasonal)

}
