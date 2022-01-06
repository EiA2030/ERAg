#' Calculate climate statistics
#'
#' The `CalcClimate` function applies a complex sequence of calculationsto ERA and climate data (e.g. `POwER.CHIRPS`) processed as per the `ERA-Climate-Data` vignette.
#' `CalcClimate` calculates seasonal and long-term average climate variables the deviance of the seasonal values from  long-term averages. `CalcClimate` can also
#'  provide bioclimatic variables annually with long-term averages and annual deviances (argument `Do.BioClim=T`). The `Observed` data.table returned in the output list shows calculates
#'  climate variables (such as growing degree days, total rainfall and dry-spells) using the ERA dataset provided and temporal windows of calculation defined using the
#'  `Win.Start` and `Windows` arguments. Two calculations windows are defined by default: `Data` which uses the planting and harvest dates reported/estimated and `EcoCrop`
#'  which uses the planting dates reported/estimated and EcoCrop estimates of season length. Additional windows of analysis (e.g. post planting 1-30 days after planting) can
#'  are defined using the `Windows` argument. If argument `Do.LT.Avg=T`estimates of long-term climate are estimated using planting dates derived from the rainfall data supplied (and not the ERA data, unless
#'  insufficient rainfall to estimate a planting date) and the arguments `Rain.Windows`, `Widths`, `Rain.Threshold` and `LongTerm`. The end years of the period for which
#'  long-term averages are calculated is set using the `Max.LT.Avg` argument. Please carefully read the **Details** and **Arguments** section of this documentation before use.
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
#' 5) If argument `Do.LT.Avg==T` annual and long-term climate statistics an planting dates are calculated  for each combination of location and product. Note
#' that each season in the data is treated separately. The process logic is as follows:
#'  i) The median planting julian day of the year is taken for the location x product combination;
#'  ii) For each year of complete climate data, the median planting date from i) is used as a starting point to estimate planting date from rainfall using the
#'  `Est.Rain` function which takes the arguments `Rain.Windows`, `Widths` and `Rain.Threshold` as explained in the arguments for this function. These data are
#'   returned as `[[LongTerm]][[LT.PD.Years]]`;
#'  iii) Estimated planting dates from ii) are averaged (mean, sd, median) across years and returned as `[[LongTerm]][[LT.PD.Avg]]`;
#'  iv) Annual planting date deviance is calculated as the absolute difference between the annual and mean/median planting dates and appended to
#'  the output of ii);
#'  v) Annual climate statistics are calculated for each year x site x product combination using the rainfall estimated planting date and as per steps
#'   3 & 4. Data are return as `[[LongTerm]][[LT.Clim.Years]]`. Where insufficient rainfall fell to meet the thresholds specified in ii) then the a median
#'   planting date is substituted from those years that did have sufficient rainfall in ii);
#'  vi) Long-term climate statistics from v) are calculated across years for mean, median, standard deviation, minimum and maximum statistics. Data are
#'   returned `[[LongTerm]][[LT.Clim.Avg]]`;
#'  vii) Deviance from long-term mean and median values for each climate statistic is appended as columns to the output of v).
#'
#'  6) If argument `Do.BioClim==T` then:
#'   i) Annual bioclimatic values are calculated using the \link[dismo]{biovars} function for each complete year of
#'    data in `CLIMATE` per unique locationas (`ID` field) in `DATA`. The output data is return as [[Annual.BioClim]][[Annual.Estimates]]`;
#'   ii) Bioclim variables are averaged over time to give long-term mean and median values. The output data is return as [[Annual.BioClim]][[LT.Averages]]`;
#'   iii) Deviance from long-term mean and median values for each climate statistic is appended as columns to the output of i).
#'
#'   If you are finding there are observations with suspiciously short reported season lengths in the ERA dataset (i.e. errors in the reporting of
#'   planting and/or harvest dates) you can exclude observations (`Exclude.EC.Diff`) where the absolute difference between reported and EcoCrop estimated season lengths
#'   is greater than a specified proportion (`EC.Diff`).
#'
#' @param DATA An ERA dataset (e.g. `ERA.Compiled`) processed by the `AddEcoCrop`, `EstPDayData`, `EstSLenData` and `EstPDayRain` functions.
#' @param CLIMATE A daily agroclimatology dataset with fields: 1) `Temp.Mean` = mean daily temperature - C; 2) `Temp.Max` = maximum daily temperature - C;
#' `Temp.Min` = minimum daily temperature - C; 4) `Rain` = daily rainfall - mm; 5) `ETo` = reference evapotranspiration - mm; 6) class `Date` field
#'  named `Date`; and 7) location identity as per `ID` argument and named the same as per `DATA`.
#' @param ID A character vector of length one containing the column name for a unique id field naming each location in the dataset provided
#' @param Rain.Data.Name A character vector of length one containing the name of the rainfall dataset in `CLIMATE`, this must be the same has the rainfall data used when preparing `DATA` using
#' the `EstPDayRain` function. This can be the same as `Temp.Data.Name`.
#' @param Temp.Data.Name A character vector of length one containing the name of the agroclimatology dataset in `CLIMATE`.
#' @param Rain.Windows An integer vector; the width in days of four temporal windows within which rainfall is assessed for potential planting dates. The first
#'  value in the series specifies a period before the planting date, then subsequent values define three consecutive windows after the planting date.
#' @param Widths An integer vector of length equivalent to length(`Window`); the width of the scanning window in days within which rainfall is summed for
#' the corresponding `Rain.Windows` entry.
#' @param Rain.Threshold An integer vector of length equivalent to length(`Rain.Windows`); the amount of rainfall that has to fall in the temporal windows considered.
#' @param Win.Start Integer value of length one; when should the window for climate calculations begin relative to the planting date (estimated or reported) in days? for example: 0 = day of planting, -1 = one day before planting, +1 = one day after planting.
#' @param Do.LT.Avg Logical `T/F;` if `T` long term averages and deviances are calculated.
#' @param Max.LT.Avg Integer of length one only relevant if `Do.LT.Avg==T`; maximum year considered when calculating long-term averages.
#' @param Do.BioClim Logical `T/F`; if `T` calculate annual and long-term average bioclimatic variables from the agroclimatic data provided in `CLIMATE` using the \link[dismo]{biovars} function
#' @param Windows A data.table with three columns `Name`, `Start` and `End` which specifies additional temporal periods for calculation of climate statistics.
#' For example `data.table(Name="Plant.1-30",Start=1,End=30)` is a temporal period from the day after planting to 30 days after planting. Set to NA if no additional windows are required.
#' @param SaveDir A character vector of length one containing the path to the directory where the output is saved. Set to NA if you do not want to save the returned datasets.
#' @param ErrorDir A character vector of length one containing the path to the directory where information on potential analysis errors is to be saved. Set to NA if you do not want to save the returned dataset.
#' @param Exclude.EC.Diff Logical `T/F`; if `T` observations with a proportional difference of greater than `EC.Diff` between reported (or data estimated) and ecocrop season length are excluded.
#' @param EC.Diff A numeric vector of length one; a value 0-1 to be used in combination with the `Exclude.EC.Diff` argument.
#' If `Exclude.EC.Diff==T` and `abs(Reported.Season.Length-EcoCrop.Season.Length)/EcoCrop.Season.Length>EC.Diff` then observations are excluded from analysis.
#' @param ROUND An integer vector of length one indicating the number of decimal places to round output values to.
#' @return A list is output containing following data.tables:
#' \enumerate{
#' \item **`[[Observed]]`** = a `data.table` of seasonal climate statistics with the fields:
#' \itemize{
#' \item`GDDlow` growing degree hours below optimal EcoCrop temperature threshold (h)
#' \item`GDDopt` growing degree hours within optimal EcoCrop temperature thresholds (h)
#' \item*`GDDhigh` growing degree hours above optimum and below maximum EcoCrop temperature thresholds (h)
#' \item`GDDmax` growing degree hours above EcoCrop maximum temperature threshold (h)
#' \item`Rain.Days.L.0` total number of days with 0 mm rainfall (days)
#' \item`Rain.Days.L.1` total number of days with less than 1 mm rainfall (days)
#' \item`Rain.Days.L.5` total number of days with less than 5 mm rainfall (days)
#' \item`Rain.Max.RSeq.0` longest continuous period of days with 0 mm rainfall (days)
#' \item`Rain.Max.RSeq.0.1` longest continuous period of days with less than 0.1 mm rainfall (days)
#' \item`Rain.Max.RSeq.1` longest continuous period of days with less than 1 mm rainfall (days)
#' \item`Rain.Max.RSeq.5` longest continuous period of days with less than 5 mm rainfall (days)
#' \item`Rain.N.RSeq.T0.D7` number of continuous periods of 7 days or more of 0 mm rainfall
#' \item`Rain.N.RSeq.T1.D7` number of continuous periods of 7 days or more of less than 1 mm rainfall
#' \item`Rain.N.RSeq.T5.D7` number of continuous periods of 7 days or more of less than 5 mm rainfall
#' \item`Rain.N.RSeq.T0.D14` number of continuous periods of 14 days or more of 0 mm rainfall
#' \item`Rain.N.RSeq.T1.D14` number of continuous periods of 14 days or more of less than 1 mm rainfall
#' \item`Rain.N.RSeq.T5.D14` number of continuous periods of 14 days or more of less than 5 mm rainfall
#' \item`Rain.N.RSeq.T0.D21` number of continuous periods of 21 days or more of 0 mm rainfall
#' \item`Rain.N.RSeq.T1.D21` number of continuous periods of 21 days or more of less than 1 mm rainfall
#' \item`Rain.N.RSeq.T5.D21` number of continuous periods of 21 days or more of less than 5 mm rainfall
#' \item`Rain.sum` total rainfall (mm)
#' \item`ETo.sum` summed Penman-Monteith reference evapotranspiration (mm)
#' \item`ETo.NA` number of NA values in Penman-Monteith reference evapotranspiration
#' \item`WBalance` `Rain.sum-ETo.sum` difference between rainfall and reference evapotranspiration (mm)
#' \item`Tmax.mean` mean of daily maximum temperatures (C)
#' \item`Tmax.sd` standard deviation of daily maximum temperatures (C)
#' \item`Tmean.mean` mean of daily mean temperatures (C)
#' \item`Tmean.sd` standard deviation of daily mean temperatures (C)
#' \item`EU` ERA experimental unit (or product) code see `ERAg::EUCodes` for translations
#' \item`PD.Used` planting date used in calculations
#' \item`W.Start` adjust of the start date of the climate calculation window as days before (negative) or after (positive) after planting date (days)
#' \item`W.End` end point of the climate calculation window as the number of days after `PD.Used + W.Start` (days)
#' \item`W.Name` name of the climate window being considered. `Data` planting and harvest dates reported in publications used to define the climate calculation window. `EcoCrop` EcoCrop database used to define season lengths (i.e. `W.End`) for the climate calculation window, unless sufficient data existed within ERA to estimate season length for the specific crop.
#' \item`ID` unique ID for site provided to the `CalcClimate` function (for `ERA.Compiled` this is usually `Site.Key`)
#' \item`M.Year` measurement year, this should correspond the to year at the end of the climate calculation window (y)
#' \item`Season` measurement season (1, 2 or NA)
#' }
#' \item**`[[LongTerm]]`**
#' \item*`[[LongTerm]][[LT.PD.Years]]`* A `data.table` of planting dates estimated from rainfall data as per process 5) with the field:
#' \itemize{
#' \item `P.Year` rainfall estimated planting year of crop (y)
#' \item `P.Date` rainfall estimated planting date of crop; class `Date` with format `%Y%m%d`
#' \item `Dev.Mean` deviance in days of planting date from **mean** long-term  planting date calculated for location (d)
#' \item `Dev.Med` deviance in days of planting date from **median** long-term planting date calculated for location (d)
#' \item `EU` ERA experimental unit (product) code see `ERAg::EUCodes` for translations
#' \item `Season` growing season of year for bimodal areas
#' \item `ID` unique ID for site provided to the `CalcClimate` function (for `ERA.Compiled` this is usually `Site.Key`)
#' \item `P.Data.Flag` indicates if no seasons in a time-series met the rainfall thresholds required for planting. If this occurs then the  mid-point of the reported planting dates is used for `P.Year` and `P.Date` fields.
#' }
#' \item*`[[LongTerm]][[LT.PD.Avg]]`* A `data.table` of long-term averages for the planting dates in `[[LongTerm]][[LT.PD.Years]]`
#' \itemize{
#' \item `Mean` mean planting date in julian days (d)
#' \item `Median` median planting date in julian days (d)
#' \item `SD` standard deviation of mean planting date (d)
#' \item `N` number of years used to calculate long-term average
#' \item other fields as per the `[[LongTerm]][[LT.PD.Years]]` table
#' }
#' \item*`[[LongTerm]][[LT.Clim.Years]]`* a `data.table` of seasonal climate statistics calculated for each row of `[[LongTerm]][[LT.PD.Years]]`. Fields are as per the `Observed` table apart from fields:
#' \itemize{
#' \item `P.Year` year of planting (y)
#' \item `H.Year` year of harvest (or end of calculation window) (y)
#' \item `Variable` in rows with `Annual.Value` statistics are calculated for the year in question, statistics in `Dev.Mean` and `Dev.Med` rows show annual deviance from long-term averages (as presented in the`[[LongTerm]][[LT.Clim.Avg]]` table)
#' }
#' \item**`[[LongTerm]][[LT.Clim.Avg]]`* a `data.table` of long-term climate statistics calculated from the `[[LongTerm]][[LT.Clim.Years]]` table. Fields are as per the `Observed` table apart from field:
#' \itemize{
#' \item `Variable` one of `Mean`, `Median`, `SD`, `Min` and `Max` indicating the function applied to each climate statistic across the temporal period for each location x crop x season
#' }
#' \item**`[[BioClim]]`** = a list of three data.tables containing bioclimatic variables derived using the \link[dismo]{biovars} function
#' \item*`[[BioClim]][[Annual.Estimates]]`* = annual estimates of bioclimatic variables for each location with fields:
#' \itemize{
#' \item `ID` unique ID for site provided to the `CalcClimate` function (for `ERA.Compiled` this is usually `Site.Key`)
#' \item `BIO1 to BIO19` bioclimatic variables, for an explanation of these codes see `ERAg::BioClimCodes`
#' \item `Variable` in rows with `Annual.Value` statistics are calculated for the year in question, statistics in `Dev.Mean` and `Dev.Med` rows show annual deviance from long-term averages (as presented in the`[[BioClim]][[LT.Averages]]` table)
#' }
#' \item*`[[BioClim]][[LT.Averages]]`* = a `data.table` of long-term climate statistics calculated from the `[[BioClim]][[Annual.Estimates]]`, fields that differ are:
#' \itemize{
#' \item `Variable` one of `Mean`, `Median` or `SD` indicating the function applied to each BIO statistic across the temporal period for each location x crop x season
#' \item `N` number of years used to calculate long-term average
#' }
#' \item *`[[Parameters]]`* = a `list` record of argument values supplied to the `CalcClimate` function
#' }
#' @export
#' @importFrom circular circular
#' @importFrom zoo rollapply as.zoo as.Date
#' @import data.table
#' @importFrom dismo biovars
CalcClimate<-function(DATA,
                      CLIMATE,
                      ID,
                      Rain.Data.Name,
                      Temp.Data.Name,
                      Rain.Windows = c(6*7,4*7,2*7,2*7),
                      Widths = c(3,3,2,2),
                      Rain.Threshold = c(30,30,20,15),
                      Win.Start = 1,
                      Do.LT.Avg=T,
                      Max.LT.Avg=2010,
                      Do.BioClim=T,
                      Windows=data.table(Name="Plant.0-30",Start=1,End=30),
                      SaveDir="Climate Stats/",
                      ErrorDir="Climate Stats/Errors/",
                      Exclude.EC.Diff=F,
                      EC.Diff=0.6,
                      ROUND=5){

  DATA<-data.table(DATA)

  DATA[Season.Start==1 & Season.End==1,M.Year.Code:="1"
  ][Season.Start==2 & Season.End==2,M.Year.Code:="2"
  ][Season.Start==1 & Season.End==2,M.Year.Code:="1&2"
  ][M.Year=="",M.Year:=NA]

  CLIMATE<-data.table(CLIMATE)

  if(!is.na(ErrorDir) & substr(ErrorDir,nchar(ErrorDir),nchar(ErrorDir))!="/"){
    ErrorDir<-paste0(ErrorDir,"/")
  }

  if(!is.na(SaveDir) & substr(SaveDir,nchar(SaveDir),nchar(SaveDir))!="/"){
    SaveDir<-paste0(SaveDir,"/")
  }

  Est.Rain<-function(Rain,Date,Widths,Rain.Threshold,Rain.Windows){

    Rain<-unlist(Rain)
    Date<-unlist(Date)
    PD.N<-Date[1]

    # Make sure the rainfall dataset is complete
    if(!sum(is.na(Rain))>0){

      R<-which(zoo::rollapply(zoo::as.zoo(Rain[1:Rain.Windows[1]]),width=Widths[1],sum)>Rain.Threshold[1])

      if(length(R)>0){
        PD.N+R[1]
      }else{
        R<-which(zoo::rollapply(zoo::as.zoo(Rain[(Rain.Windows[1]+1):sum(Rain.Windows[1:2])]),width=Widths[2],sum)>Rain.Threshold[2])
        if(length(R)>0){
          PD.N+R[1]
        }else{
          R<-which(zoo::rollapply(zoo::as.zoo(Rain[(sum(Rain.Windows[1:2])+1):sum(Rain.Windows[1:3])]),width=Widths[3],sum)>Rain.Threshold[3])
          if(length(R)>0){
            PD.N+R[1]
          }else{
            R<-which(zoo::rollapply(zoo::as.zoo(Rain[(sum(Rain.Windows[1:3])+1):sum(Rain.Windows[1:4])]),width=Widths[4],sum)>Rain.Threshold[4])
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

    C.Val<-circular::circular(as.numeric(format(as.Date(paste0("2000",substr(as.character(X), 5, 10))),"%j"))*360/365*pi/180)[[1]]
    N<-round(as.numeric(FUN(C.Val)*180/pi*365/360),0)

    if(length(X)>1){
      if(N<=0){
        sprintf("%03d",365+N)
      }else{
        sprintf("%03d",N)
      }
    }else{
      sprintf("%03s",format(X,"%j"),N)
    }
  }

  SLen<-function(RName,DATA,ErrorDir,EC.Diff,Exclude.EC.Diff){
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

    if(Exclude.EC.Diff){
      SS<-SS[!((abs(SLen.Merge-SLen.EcoCrop)/SLen.EcoCrop)>EC.Diff & !((is.na( SLen.Merge) & !is.na(SLen.EcoCrop))) | (!is.na( SLen.Merge) & is.na(SLen.EcoCrop))),][!(is.na(SLen.Merge) & is.na(SLen.EcoCrop)),]
    }

    return(list(SS=SS,DATA=DATA))
  }

  # Analysis parameter code for cross-reference of saved data to current analysis
  Params<-paste0(paste0(Rain.Windows,collapse=""),
                 paste0(Widths,collapse=""),
                 paste0(Rain.Threshold,collapse=""),
                 Max.LT.Avg,
                 paste(apply(Windows[,2:3],1,paste,collapse=""),collapse = ""),
                 Win.Start,
                 substr(Temp.Data.Name,1,3),
                 substr(Rain.Data.Name,1,3),
                 Exclude.EC.Diff,
                 EC.Diff)

  if(!is.na(SaveDir)){
    SaveDir1<-paste0(SaveDir,"Analysis/",Params,"/")
    # Create Save Directory as Required
    if(!dir.exists(SaveDir1)){
      dir.create(SaveDir1,recursive = T)
    }
  }

  # Create a text file detailing parameters used in analysis
  if(!is.na(SaveDir)){
    ParamSave<-list(Rain.Windows=c("Rain.Windows:",Rain.Windows),
                    Widths=c("Widths:",Widths),
                    Rain.Threshold=c("Rain.Threshold:",Rain.Threshold),
                    Max.LT.Avg=c("Max.LT.Avg:",Max.LT.Avg),
                    Windows=c("Windows:",unlist(Windows)),
                    Win.Start=Win.Start,
                    Rain.Data.Name=c("Rain.Data.Name:",Rain.Data.Name),
                    Temp.Data.Name=c("Temp.Data.Name:",Temp.Data.Name),
                    Exclude.EC.Diff=Exclude.EC.Diff,
                    EC.Diff=EC.Diff)

    if(file.exists(paste0(SaveDir1,"/parameters.txt"))){
      unlink(paste0(SaveDir1,"/parameters.txt"))
    }

    lapply(ParamSave, cat, "\n", file=paste0(SaveDir1,"parameters.txt"), append=TRUE)
  }


  DATA<-data.table(DATA)

  DATA[,Plant.Start:=if(class(Plant.Start)=="Date"){Plant.Start}else{as.Date(Plant.Start,"%d.%m.%Y")}
  ][,Plant.End:=if(class(Plant.End)=="Date"){Plant.End}else{as.Date(Plant.End,"%d.%m.%Y")}
  ][,Harvest.Start:=if(class(Harvest.Start)=="Date"){Harvest.Start}else{as.Date(Harvest.Start,"%d.%m.%Y")}
  ][,Harvest.End:=if(class(Harvest.End)=="Date"){Harvest.End}else{as.Date(Harvest.Start,"%d.%m.%Y")}]

  # Refine Season Length where uncertainty in the timing of planting or harvest exists
  DATA[,PDiff:=Plant.End-Plant.Start][,H.Diff:=Harvest.End-Harvest.Start]

  # Create unique Site x EU x Date combinations
  SS<-SLen(RName=Rain.Data.Name,DATA,ErrorDir,EC.Diff,Exclude.EC.Diff)
  DATA<-SS$DATA
  SS<-SS$SS
  MCode.SS<-apply(SS[,c(..ID,"EU","P.Date.Merge","M.Year")],1,paste,collapse="_")

  SaveName<-paste0(SaveDir1,"ClimStatsA.RData")

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
            Start=rep(Win.Start,2),
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
            cat('\r',paste0(Temp.Data.Name," x ",Rain.Data.Name,": Estimating Long-term averages for  site: ",match(Site,Sites),"/",length(Sites)," | Season: ",EU.N.S$M.Year.Code[i]," | EU: ",EU.N.S$EU[i]))
            flush.console()

            PDates<-as.Date(paste0(Year.Range,"-",SS.N[EU==EU.N.S$EU[i] & M.Year.Code==EU.N.S$M.Year.Code[i],C.Med(P.Date.Merge,FUN=stats::median)]),format="%Y-%j")

            # Excise temporal windows from climate data
            C1<-data.table(Start=which(Climate$Date %in% PDates)-Rain.Windows[1])[,End:=Start+sum(Rain.Windows)][!Start<=0][!End>nrow(Climate)]
            C<-Climate[unlist(lapply(1:nrow(C1),FUN=function(l){unlist(C1[l,1]):unlist(C1[l,2])})),][!is.na(Year)]

            C[,Sequence:=C1[,Nrow:=1:nrow(C1)][,rep(Nrow,length(Start:End)),by=Nrow][,V1]]

            # Remove incomplete sequences by cross referencing the width the window should be
            C<-C[Sequence %in%  C[,.N == sum(Rain.Windows)+1,by=Sequence][,Sequence]]

            # Add "data" year
            C[,YearX:=Year[1],by=Sequence]

            # Work out planting date for each sequence
            XX<-C[,Est.Rain(Rain,Date,..Widths,..Rain.Threshold,..Rain.Windows),by=list(YearX,Sequence)]

            Annual.Plant<-zoo::as.Date(unlist(XX[,3]))
            names(Annual.Plant)<-XX[,YearX]


            # Record the planting year of each sequence (the planting date used as the starting point to search from, i.e. PDates)
            P.Years<-unlist(C[c(1, cumsum(rle(C$Sequence)$lengths) + 1)+Rain.Windows[1],"Year"][!is.na(Year)])

            N<-!is.na(Annual.Plant)
            N1<-!is.na(Annual.Plant) &  as.integer(format(Annual.Plant,"%Y"))<=Max.LT.Avg

            No.PDate.Flag<-""

            if(sum(N1)==0){
              Annual.Plant<-rep(as.Date(NA),length(P.Years))
              names(Annual.Plant)<-P.Years
              PD.Date.Pub<-SS.N[EU==EU.N.S[i,EU] &  M.Year.Code==EU.N.S[i,M.Year.Code] & P.Date.Merge>="1983-07-01",P.Date.Merge]
              Annual.Plant[match(format(PD.Date.Pub,"%Y"),Year.Range)]<-PD.Date.Pub

              No.PDate.Flag<-"No seasons met rainfall threshold, mid-point of published planting period used."
              C.val<-circular::circular(as.numeric(format(base::as.Date(paste0("2000",substr(as.character(Annual.Plant[!is.na(Annual.Plant)]), 5, 10))),"%j"))*360/365*pi/180)
            }else{
              C.val<-circular::circular(as.numeric(format(base::as.Date(paste0("2000",substr(as.character(Annual.Plant[N1]), 5, 10))),"%j"))*360/365*pi/180)
            }


            LT.Mean<-as.numeric(round(mean(C.val)*180/pi*365/360,ROUND))
            LT.Median<-as.numeric(round(median(C.val)*180/pi*365/360,ROUND))

            if(LT.Mean<=0){
              LT.Mean<-365+LT.Mean
            }

            if(LT.Median<=0){
              LT.Median<-365+LT.Median
            }

            # Calculate LT.Avg Planting date standard deviation
            LT.SD<-round(sd(C.val)*180/pi*365/360,ROUND)

            # Calculate Annual Deviation of Planting Date From LT.Avg
            # Annual Estimate - Long Term Average
            if(sum(N1)==0){
              Annual.Dev.Mean<-NA
              Annual.Dev.Median<-NA
            }else{
              Annual.Dev.Mean<-as.numeric(circular::circular(as.numeric(format(Annual.Plant,"%j"))*360/365*pi/180)-circular::circular(as.numeric(LT.Mean)*360/365*pi/180))*365/360*180/pi

              Annual.Dev.Median<-as.numeric(circular::circular (as.numeric(format(Annual.Plant,"%j"))*360/365*pi/180)- # Annual Estimate
                                              circular::circular(as.numeric(LT.Median)*360/365*pi/180)[[1]]        # Long Term Average
              )*365/360*180/pi
            }

            NYear.LT.Avg<-sum(N1)

            LT.PlantDates<-list(Annual.Estimates=data.table(P.Year=P.Years,
                                                            P.Date=Annual.Plant,
                                                            Dev.Mean=Annual.Dev.Mean,
                                                            Dev.Med=Annual.Dev.Median,
                                                            EU=EU.N.S$EU[i],
                                                            Season=EU.N.S$M.Year.Code[i],
                                                            ID=Site,
                                                            P.Data.Flag=No.PDate.Flag),
                                LT.Averages=data.table(Mean=LT.Mean,
                                                       Median=LT.Median,
                                                       SD=LT.SD,N=NYear.LT.Avg,
                                                       EU=EU.N.S$EU[i],
                                                       Season=EU.N.S$M.Year.Code[i],
                                                       ID=Site,
                                                       P.Data.Flag=No.PDate.Flag))

            # Now Estimate Temperature and Rainfall LTAvgs using the average season lengths and pre-set windows
            WINS<-rbind(data.table(
              Name=c("Data","EcoCrop"),
              Start=rep(Win.Start,2),
              End=c(SS.N[i,SLen.Merge][1], SS.N[i,SLen.EcoCrop][1])),
              Windows
            )[,End:=round(End,0)][,Start:=round(Start,0)]

            WINS<-WINS[!is.na(End),]

            if(nrow(WINS)>0){

              # For seasons that do not meet planting thresholds then substitute median planting date from LT data
              Annual.Plant[is.na(Annual.Plant)]<- zoo::as.Date(paste0(names(Annual.Plant[is.na(Annual.Plant)]),"-",round(LT.Median)),"%Y-%j")

              LT.Climate<-lapply(1:nrow(WINS),FUN=function(k){

                A.Harvest<-Annual.Plant+round(WINS[k,End])
                A.Plant<-Annual.Plant+WINS[k,Start]

                A.P.H<-A.Plant %in% Climate$Date & A.Harvest %in% Climate$Date
                A.Harvest<-A.Harvest[A.P.H]
                A.Plant<-A.Plant[A.P.H]

                N1<-which(Climate$Date %in% A.Plant)
                N2<-which(Climate$Date %in% A.Harvest)

                C<-rbindlist(lapply(1:length(N1),FUN=function(i){
                  X<-Climate[N1[i]:N2[i]]
                  X[,P.Year:=X[1,Year]]
                  X[,H.Year:=X[nrow(X),Year]]

                  # Check that climate dataset is complete
                  if(nrow(X)>=floor(WINS[k,End])){
                    X
                  }else{
                    NULL
                  }
                }))

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
                                                                          max(X,na.rm=T))}),.SDcols=3:ncol(C)
                ][,Variable:=c("Mean","Median","SD","Min","Max")
                ][,N:=nrow(C[H.Year<=Max.LT.Avg,1])]

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
                LT.Avg$P.Data.Flag<-No.PDate.Flag

                Annual.Estimates$EU<-EU.N.S$EU[i]
                Annual.Estimates$Season<-EU.N.S$M.Year.Code[i]
                Annual.Estimates$ID<-Site
                Annual.Estimates$W.Start<-WINS[k,Start]
                Annual.Estimates$W.End<-WINS[k,End]
                Annual.Estimates$W.Name<-WINS[k,Name]
                Annual.Estimates$P.Data.Flag<-No.PDate.Flag

                list(Annual.Estimates=Annual.Estimates,LT.Averages=LT.Avg)

              })
              names(LT.Climate)<-WINS[,Name]
            }else{
              LT.Climate<-NA
            }

            list(LT.PlantDates=LT.PlantDates,LT.Climate=LT.Climate)



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
        fwrite(data.table(Missing=unique(c(MissingLines,MCode))),paste0(SaveDir1,"MissingLines.csv"))
      }else{
        fwrite(data.table(Missing=unique(MCode)),paste0(SaveDir1,"MissingLines.csv"))
      }
    }

    if(Do.LT.Avg){

      # Deal with NA values in LT climate data
      for(i in 1:length(B)){
        X<-B[[i]][["LT Avg & Dev"]]
        X<-X[!unlist(lapply(X,FUN=function(Z){is.na(Z)[1]}))]
        B[[i]][["LT Avg & Dev"]]<-X
      }

      # Rbind LT climate data stats together from lists

      LT.PD.Years<-rbindlist(lapply(1:length(B),FUN=function(i){
        X<-B[[i]][["LT Avg & Dev"]]
        X<-rbindlist(lapply(X,"[[",c("LT.PlantDates","Annual.Estimates")))
        X

      }))

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

    SaveName<-paste0(SaveDir1,"ClimStatsB.RData")

    # Cross-reference to exisiting data
    S.existing1<-NULL
    MCode1<-NULL
    MissingSites<-NULL
    MCode.D<-NULL

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

      BIOV.LT<-BIOV[Year<=Max.LT.Avg,lapply(.SD,FUN=function(X){round(c(mean(X),median(X),sd(X)),2)}),by="ID",.SDcol=3:ncol(BIOV)
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


      BIOV<-list(Annual.Estimates=Annual.Estimates,LT.Averages = BIOV.LT)

      if(!is.na(SaveDir1)){
        save(BIOV,file=SaveName)
      }

      Seasonal$BioClim<-BIOV
    }else{
      Seasonal$BioClim<-S.existing1
    }
  }

  # Save Parameters
  ParamSave<-list(Rain.Windows=Rain.Windows,
                  Widths=Widths,
                  Rain.Threshold=Rain.Threshold,
                  Max.LT.Avg=Max.LT.Avg,
                  Windows=Windows,
                  Win.Start=Win.Start,
                  Rain.Data.Name=Rain.Data.Name,
                  Temp.Data.Name=Temp.Data.Name,
                  Exclude.EC.Diff=Exclude.EC.Diff,
                  EC.Diff=EC.Diff)

  if(!is.na(SaveDir)){
    save(ParamSave,file=paste0(SaveDir1,"parameters.RData"))
  }
  Seasonal$Parameters<-ParamSave

  # Save Dataset Used
  if(!is.na(SaveDir)){
    save(DATA,file=paste0(SaveDir1,"Data.RData"))
  }

  Seasonal$DATA<-DATA

  return(Seasonal)

}


