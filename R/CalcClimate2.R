#' Calculate climate statistics
#'
#' The `CalcClimate` function applies a complex sequence of calculations to site level climate data extracted for ERA observations.
#' The `ERA-Climate-Data` vignette describes how to construct the datasets that need to be supplied to  `CalcClimate`, at this time the vignette is outdated but revision is planned.
#' `CalcClimate` calculates seasonal and long-term average climate variables. The deviance of seasonal values from  long-term averages is also calculated. `CalcClimate` can also
#'  provide bioclimatic variables annually with long-term averages and annual deviance (argument `Do.BioClim=T`). The `Observed` data.table returned in the output list shows calculated
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
#' 4) For each temporal period specified in multiple climate statistics are calculated:
#'  i) **growing degrees days (GDD)** are calculated using the `GDDcalc` function which requires daily `Tmax` and `Tmin` fields from `CLIMATE` and the EcoCrop
#'   optimal and absolute temperate thresholds (fields `Tlow`, `Topt.low`, `Topt.high`, and `Thigh`) added to `DATA` using the `AddEcoCrop` function.
#'   The `GDDcalc` function outputs four fields `GDDlow`, `GDDopt`, `GDDhigh` and `GDDmax` and the values of these are summed for the temporal period;
#'  ii) **precipitation and reference evapotranspiration (ETo)** statistics are estimated using the `RAIN.Calc` function which outputs a number of variables
#'   described in the documentation for this function;
#'  iii) **min, max and mean daily temperatures and rainfall plus variance**;
#'  iv) Using the `Rain.Threshold` and `Temp.Threshold` parameters thresholds and their direction (greater or lower) can be specified for rainfall and
#'  minimum and maximum temperatures. Within a temporal period the total number of threshold exceedance days are calculated. The `RSeqLen` and `TSeqLen`
#'  parameters define sequence lengths (e.g. 5 days), for each value of these parameters the number of sequences of consecutive days in exceedance of rainfall or
#'  temperature thresholds equal to higher than the specified sequence length are summed. If the rainfall threshold is `1` with direction `lower` and `RSeqLen`
#'  is 5, then the number of sequences of consecutive days with less than 1mm rainfall of sequence length 5 days or more are counted. Note in the previous
#'  scenario a sequence of length 30 days would only count as one sequence;
#'  v) Similar to iv), the number of days and sequences of days (`ERSeqLen`) with `Eratio` less than the thresholds set in `ER.Threshold` are calculated. Mean, median and minimum `ERatio` values are also calculated; and
#'  vi) Similar to iv), the number of days and sequences of days (`LSeqLen`) where `Logging` exceeds `0`, `0.5*ssat` or `ssat` are calculated. Summed, mean, median and maximum `Logging` values are also calculated
#'
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
#'   returned `[[LongTerm]][[LT.Clim.Avg]]`; and
#'  vii) Deviance from long-term mean and median values for each climate statistic is appended as columns to the output of v).
#'
#'  6) If argument `Do.BioClim==T` then:
#'   i) Annual bioclimatic values are calculated using the \link[dismo]{biovars} function for each complete year of
#'    data in `CLIMATE` per unique locationas (`ID` field) in `DATA`. The output data is return as [[Annual.BioClim]][[Annual.Estimates]]`;
#'   ii) Bioclim variables are averaged over time to give long-term mean and median values. The output data is return as [[Annual.BioClim]][[LT.Averages]]`; and
#'   iii) Deviance from long-term mean and median values for each climate statistic is appended as columns to the output of i).
#'
#'   If you are finding there are observations with suspiciously short reported season lengths in the ERA dataset (i.e. errors in the reporting of
#'   planting and/or harvest dates) you can exclude observations (`Exclude.EC.Diff`) where the absolute difference between reported and EcoCrop estimated season lengths
#'   is greater than a specified proportion (`EC.Diff`).
#'
#' @param Data a data.frame or data.table of sites x products (crops) with fields for planting, season length and thermal optima:
#' \itemize{
#'  \item{`ID`}{a character field containing a unique code for each location in the dataset, this field is key that links to information in the `CLIMATE` dataset, the field name is given by the `ID` parameter}
#'  \item{`EU`}{a character field containing an ERA product code}
#'  \item{`Product`}{a character field containing the product name}
#'  \item{`M.Year`}{an integer field for the planting year (Y)}
#'  \item{`M.Season`}{an integer field of planting season,`1, 2, or NA`, for locations with more than one growing season, if only one growing season is present at location use `NA`}
#'  \item{`PlantingDate`}{a class `Date` field for date of planting (yyyy-mm-dd)}
#'  \item{`SeasonLength.Data`}{an integer field for observed length of growing season/cycle (d)}
#'  \item{`SeasonLength.EcoCrop`}{na integer field for EcoCrop growing season/cycle (d)}
#'  \item{`Topt.low`}{a numeric field containing the lower absolute thermal limit for product}
#'  \item{`Topt.high`}{a numeric field containing the upper absolute thermal limit for product}
#'  \item{`Tlow`}{a numeric field containing the lower absolute thermal limit for product}
#'  \item{`Thigh`}{a numeric field containing the upper absolute thermal limit for product}
#' }
#'@param CLIMATE A daily agroclimatology dataset with fields:
#'\itemize{
#'  \item{`ID`}{a character field containing a unique code for each location in the dataset, this field is key that links to information in the `Data` dataset, the field name is given by the `ID` parameter}
#'  \item{`Temp.Mean`}{a numeric field containing mean daily temperature (C)}
#'  \item{`Temp.Max`}{a numeric field containing maximum daily temperature (C)}
#'  \item{`Temp.Min`}{a numeric field containing minimum daily temperature (C)}
#'  \item{`Rain`}{a numeric field containing daily rainfall (mm)}
#'  \item{`ETo`}{a numeric field containing reference evapotranspiration (mm)}
#'  \item{`Date`}{a class `Date` field containing the date of observation (yyyy-mm-dd)}
#'  \item{`ERATIO`}{a numeric field containing the ratio of actual to potential evapotranspiration. See \href{https://github.com/fabiolexcastro/Gates-smallholder-adaptation/tree/master/watbal}{this github} for details on how this is calculated}
#'  \item{`LOGGING`}{a numeric field containing an estimate of waterlogging (mm). See \href{https://github.com/fabiolexcastro/Gates-smallholder-adaptation/tree/master/watbal}{this github} for details on how this is calculated}
#'  \item{`ssat`}{a numeric field containing the soil saturation value (mm). See \href{https://github.com/fabiolexcastro/Gates-smallholder-adaptation/tree/master/watbal}{this github} for details on how this is calculated}
#'}
#' @param ID A character vector of length one containing the column name for a unique id field naming each location in the dataset provided
#' @param LoadExisting Logical `T/F`; If the analysis has already been conducted given the parameters supplied then setting this parameter to `T` will load the pre-existing data.
#' @param Rain.Data.Name A character vector of length one containing the name of the rainfall dataset in `CLIMATE`, this must be the same has the rainfall data used when preparing `DATA` using
#' the `EstPDayRain` function. This can be the same as `Temp.Data.Name`.
#' @param Temp.Data.Name A character vector of length one containing the name of the agroclimatology dataset in `CLIMATE`.
#' @param Rain.Windows An integer vector; the width in days of four temporal windows within which rainfall is assessed for potential planting dates. The first
#'  value in the series specifies a period before the planting date, then subsequent values define three consecutive windows after the planting date.
#' @param Rain.Window.Widths An integer vector of length equivalent to length(`Window`); the width of the scanning window in days within which rainfall is summed for
#' the corresponding `Rain.Windows` entry.
#' @param Rain.Window.Threshold An integer vector of length equivalent to length(`Rain.Windows`); the amount of rainfall that has to fall in the temporal windows considered.
#' @param Temp.Threshold A data.table with two fields. The `Threshold` field must contain integer values of max or min temperatures, for a given day in a climate window if these thresholds are exceeded then
#' the day is counted towards the relevant climate statistics. The `Direction` field should be of class character and contain only the values `higher` or `lower` corresponding to the direction of exceedance for the thresholds provided in in the `Thresholds` field. For example,
#' if row 1 is `30` and `higher` then days where the max temperature is >30 will be counted. Similarly if row 2 is `20` and `lower` then days where the min temperature is <20 will be counted.
#' @param TSeqLen A integer vector of sequence lengths (days). For each row of `Temp.Threshold` (`i`) and each value of `TSeqLen` (`j`) the number of consecutive day sequences where `Temp.Threshold[i]` is exceeded for `TSeqLen[j]` days or more is calculated.
#' @param Rain.Threshold A data.table with two fields. The `Threshold` field must contain integer values of rainfall, for a given day in a climate window if these thresholds are exceeded then
#' the day is counted towards the relevant climate statistics. The `Direction` field should be of class character and contain only the values `higher` or `lower` corresponding to the direction of exceedance for the thresholds provided in in the `Thresholds` field. For example,
#' if row 1 is `1` and `lower` then days where rainfall is <1 will be counted.
#' @param RSeqLen A integer vector of sequence lengths (days). For each row of `Rain.Threshold` (`i`) and each value of `RSeqLen` (`j`) the number of consecutive day sequences where `Rain.Threshold[i]` is exceeded for `TSeqLen[j]` days or more is calculated.
#' @param ER.Threshold An numeric vector of thresholds between 0 and 1. The function will look for days with `ERatio` below these thresholds.
#' @param ERSeqLen A integer vector of sequence lengths (days). For each value of `ER.Threshold` (`i`) and each value of `ERSeqLen` (`j`) the number of consecutive day sequences where `ER.Threshold[i]` is exceeded for `ERSeqLen[j]` days or more is calculated.
#' @param LSeqLen A integer vector of sequence lengths (days). For preset waterlogging thresholds (`i`) vs each value of `LSeqLen` (`j`) the number of consecutive day sequences where waterlogging thresholds are exceeded for `LSeqLen[j]` days or more is calculated.
#' @param PrePlantWindow A integer vector; defines a period before planting in days for which rainfall and Penman-Monteith reference evapotranspiration are summed.
#' @param Win.Start Integer value of length one; when should the window for climate calculations begin relative to the planting date (estimated or reported) in days? for example: 0 = day of planting, -1 = one day before planting, +1 = one day after planting.
#' @param Do.LT.Avg Logical `T/F`; if `T` long term averages and deviances are calculated.
#' @param Max.LT.Avg Integer of length one only relevant if `Do.LT.Avg==T`; maximum year considered when calculating long-term averages.
#' @param Do.BioClim Logical `T/F`; if `T` calculate annual and long-term average bioclimatic variables from the agroclimatic data provided in `CLIMATE` using the \link[dismo]{biovars} function
#' @param Windows A data.table with three columns `Name`, `Start` and `End` which specifies additional temporal periods for calculation of climate statistics.
#' For example `data.table(Name="Plant.1-30",Start=1,End=30)` is a temporal period from the day after planting to 30 days after planting. Set to NA if no additional windows are required.
#' @param SaveDir A character vector of length one containing the path to the directory where the output is saved. Set to NA if you do not want to save the returned datasets.
#' @param ErrorDir A character vector of length one containing the path to the directory where information on potential analysis errors is to be saved. Set to NA if you do not want to save the returned dataset.
#' @param ROUND An integer vector of length one indicating the number of decimal places to round output values to.
#' @param DebugMode Logical `T/F`; if `T` function progress is shown line by line in the consule, if `F` progress is kept to a single line and continously overwritten.
#' @return A list is output containing following data.tables:
#' \enumerate{
#' \item **`[[Observed]]`** = a `data.table` of seasonal climate statistics. Characters in parenthesis indicate SI units
#' \itemize{
#' \item`GDDlow` growing degree hours below optimal EcoCrop temperature threshold (h)
#' \item`GDDopt` growing degree hours within optimal EcoCrop temperature thresholds (h)
#' \item`GDDhigh` growing degree hours above optimum and below maximum EcoCrop temperature thresholds (h)
#' \item`GDDmax` growing degree hours above EcoCrop maximum temperature threshold (h)
#' \item`Rain.L#.Days` or `Rain.G#.Days` total number of days with less than `Rain.L#` or greater than `Rain.G#` mm rainfall as per thresholds specified in `Rain.Threshold` (d)
#' \item`Rain.L#.Days.Pr` or `Rain.G#.Days.Pr` as per above (`Rain.G#.Days`), but divided by total length of climate window
#' \item`Rain.L#.Max.RSeq` or `Rain.G#.Max.RSeq` longest continuous period of days with less than `Rain.L#` or greater than `Rain.G#` mm rainfall as per thresholds specified in `Rain.Threshold` (d)
#' \item`Rain.L#.N.RSeq.D#` or `Rain.G#.N.RSeq.D#` number of continuous periods of `RSeq.D#` days with less than `Rain.L#` or greater than `Rain.G#` mm rainfall as per thresholds specified in `Rain.Threshold` (d)
#' \item`Rain.sum` total rainfall (mm)
#' \item`ERatio.mean` mean ratio of actual to potential evapotranspiration (`ERATIO`) for the growing season
#' \item`ERatio.median` median ratio of actual to potential evapotranspiration (`ERATIO`) for the growing season
#' \item`ERatio.min` minimum ratio of actual to potential evapotranspiration (`ERATIO`) for the growing season
#' \item`ERatio.L#.Days` total number of days `ERATIO` less than `ERatio.L#` as per thresholds specified in `ER.Threshold` (d)
#' \item`ERatio.L#.Days.Pr` as above (`ERatio.L#.Days`), but divided by total length of climate window
#' \item`ERatio.L#.Max.Seq` longest continuous period of days with `ERATIO` less than `ERatio.L#` threshold as specified in `ER.Threshold` (d)
#' \item`ERatio.L#.N.Seq.D#` number of continuous periods of `Seq.D#` days with `ERATIO` less than `ERatio.L#` threshold as specified in `ER.Threshold` (d)
#' \item`Logging.sum` sum of waterlogging (`LOGGING`) for the growing season (mm)
#' \item`Logging.mean` mean of waterlogging (`LOGGING`) for the growing season (mm)
#' \item`Logging.present.mean` mean of waterlogging for days where waterlogging is present (`LOGGING>0`) for the growing season (mm)
#' \item`Logging.median` median of waterlogging (`LOGGING`) for the growing season (mm)
#' \item`Logging.max` maximum of waterlogging (`LOGGING`) for the growing season (mm)
#' \item`Logging.Gssat#Days` total number of days with `LOGGING` more than or equal to 50% `ssat0.5` or 100% waterlogging `ssat` (d)
#' \item`Logging.Gssat#Days.Pr` as above (`Logging.Gssat#Days`), but divided by total length of climate window
#' \item`Logging.Gssat#.Max.Seq` longest continuous period of days with `LOGGING` more than or equal to 50% `ssat0.5` or 90% waterlogging `ssat0.9` (d)
#' \item`Logging.Gssat#.N.Seq.D#`  number of continuous periods of `Seq.D#` days with `LOGGING` more than or equal to 50% `ssat0.5` or 100% waterlogging `ssat` (d)
#' \item`Logging.G0Days` as per `Logging.Gssat#Days` but with threshold >0 (d)
#' \item`Logging.G0Days.Pr`  as per `Logging.Gssat#Days.Pr` but with threshold >0
#' \item`Logging.G0.Max.Seq`  as per `Logging.Gssat#.Max.Seq` but with threshold >0 (d)
#' \item`Logging.G0.N.Seq.D#`  as per `Logging.Gssat#.N.Seq.D#` but with threshold >0 (d)
#' \item`ETo.sum` summed Penman-Monteith reference evapotranspiration (mm)
#' \item`ETo.NA` number of NA values in Penman-Monteith reference evapotranspiration
#' \item`WBalance` `Rain.sum-ETo.sum` difference between rainfall and reference evapotranspiration (mm)
#' \item`WBalance.NegDays` `sum((Rain-ETo)<0)` the number of days where the difference between rainfall and reference evapotranspiration (mm) is negative (d)
#' \item`Tmin.L#.Days` or `Tmax.G#.Days` total number of days with minimum temperatures less than `Tmin.L#` or maximum temperatures greater than `Tmax.G#` as per parameters specified in `Temp.Threshold` (d)
#' \item`Tmin.L#.Days.Pr` or `Tmax.G#.Days.Pr` as per above, but divided by total length of climate window (d)
#' \item`Tmin.L#.Max.RSeq` or `Tmax.G#.Max.RSeq` longest continuous period of days with minimum temperatures less than `Tmin.L#` or maximum temperatures greater than `Tmax.G#` as per parameters specified in `Temp.Threshold` (d)
#' \item`Tmin.L#.N.RSeq.D#` or `Tmax.G#.N.RSeq.D#` number of continuous periods of `D#` days with minimum temperatures less than `Tmin.L#` or maximum temperatures greater than `Tmax.G#` as per parameters specified in `Temp.Threshold` (d)
#' \item`Tmin.min` min temp minimum (C)
#' \item`Tmin.mean` min temp mean (C)
#' \item`Tmin.var` min temp variance (C)
#' \item`Tmin.sd` min temp standard deviation (C)
#' \item`Tmin.range` min temp range(C)
#' \item`Tmax.max` max temp maximum (C)
#' \item`Tmax.mean` max temp mean (C)
#' \item`Tmax.var` max temp variance (C
#' \item`Tmax.sd` max temp standard deviation (C))
#' \item`Tmax.range` max temp range (C)
#' \item`Tmean.max` max of daily mean temperatures (C)
#' \item`Tmean.min` min of daily mean temperatures (C)
#' \item`Tmean.mean` mean of daily mean temperatures (C)
#' \item`Tmean.var` mean temp variance (C)
#' \item`Tmean.sd` standard deviation of daily mean temperatures (C)
#' \item`Tmean.range` mean temp range (C)
#' \item`Rain.sum.PrePlant#` the sum of rainfall for a period `#` days before planting to the day before planting (defined in the `PrePlantWindow` parameter)
#' \item`ETo.sum.PrePlant#`the sum of Penman-Monteith reference evapotranspiration for a period `#` days before planting to the day before planting (defined in the `PrePlantWindow` parameter)
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
CalcClimate2<-function(Data,
                       CLIMATE,
                       ID,
                       Rain.Data.Name,
                       Temp.Data.Name,
                       Rain.Windows = c(6*7,4*7,2*7,2*7),
                       Rain.Window.Widths = c(3,3,2,2),
                       Rain.Window.Threshold = c(30,30,20,15),
                       Temp.Threshold = data.table(Threshold=c(20,30,35),Direction=c("lower","higher","higher")),
                       TSeqLen = c(5,10,15),
                       Rain.Threshold = data.table(Threshold=c(0.1,1,5),Direction=c("lower","lower","lower")),
                       RSeqLen = c(5,10,15),
                       ER.Threshold=c(0.5,0.25,0.1),
                       ERSeqLen=c(5,10,15),
                       LSeqLen=c(5,10,15),
                       PrePlantWindow=10,
                       Win.Start = 1,
                       Do.LT.Avg=T,
                       Max.LT.Avg=2010,
                       Do.BioClim=T,
                       Windows=data.table(Name="Plant.1-30",Start=1,End=30),
                       SaveDir="Climate Stats/",
                       ErrorDir="Climate Stats/Errors/",
                       EC.Diff=0.6,
                       ROUND=5,
                       DebugMode=F){

  # Analysis parameter code for cross-reference of saved data to current analysis
  Params<-paste0(paste0(Rain.Windows,collapse=""),
                 paste0(Rain.Window.Widths,collapse=""),
                 paste0(Rain.Window.Threshold,collapse=""),
                 gsub("higher","H",gsub("lower","L",paste0(apply(Rain.Threshold,1,paste,collapse=""),collapse=""))),
                 paste0(RSeqLen,collapse=""),
                 gsub("higher","H",gsub("lower","L",paste0(apply(Temp.Threshold,1,paste,collapse=""),collapse=""))),
                 paste0(TSeqLen,collapse=""),
                 Max.LT.Avg,
                 PrePlantWindow,
                 paste(apply(Windows[,2:3],1,paste,collapse=""),collapse = ""),
                 Win.Start,
                 substr(Temp.Data.Name,1,3),
                 substr(Rain.Data.Name,1,3),
                 if(Do.LT.Avg){"T"}else{"F"},
                 if(Do.BioClim){"T"}else{"F"})


  if(!is.na(SaveDir)){
    # Create Save Directory if required
    if(!dir.exists(SaveDir)){
      dir.create(SaveDir,recursive = T)
    }

    if(substr(SaveDir,nchar(SaveDir),nchar(SaveDir))!="/"){
      SaveDir<-paste0(SaveDir,"/")
    }

  }


  if(!is.na(ErrorDir) & substr(ErrorDir,nchar(ErrorDir),nchar(ErrorDir))!="/"){
    ErrorDir<-paste0(ErrorDir,"/")
  }


  Est.Rain<-function(Rain,Date,Widths,Rain.Window.Threshold,Rain.Windows){

    Rain<-unlist(Rain)
    Date<-unlist(Date)
    PD.N<-Date[1]

    # Make sure the rainfall dataset is complete
    if(!sum(is.na(Rain))>0){

      R<-which(zoo::rollapply(zoo::as.zoo(Rain[1:Rain.Windows[1]]),width=Rain.Window.Widths[1],sum)>Rain.Window.Threshold[1])

      if(length(R)>0){
        PD.N+R[1]
      }else{
        R<-which(zoo::rollapply(zoo::as.zoo(Rain[(Rain.Windows[1]+1):sum(Rain.Windows[1:2])]),width=Rain.Window.Widths[2],sum)>Rain.Window.Threshold[2])
        if(length(R)>0){
          PD.N+R[1]
        }else{
          R<-which(zoo::rollapply(zoo::as.zoo(Rain[(sum(Rain.Windows[1:2])+1):sum(Rain.Windows[1:3])]),width=Rain.Window.Widths[3],sum)>Rain.Window.Threshold[3])
          if(length(R)>0){
            PD.N+R[1]
          }else{
            R<-which(zoo::rollapply(zoo::as.zoo(Rain[(sum(Rain.Windows[1:3])+1):sum(Rain.Windows[1:4])]),width=Rain.Window.Widths[4],sum)>Rain.Window.Threshold[4])
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

  RAIN.Calc<-function(Rain,ETo,Thresholds,RSeqLen){

    ZDays<-function(Data,Threshold=0,FUN=max,Direction="lower"){
      # Direction = lower or higher than the threshold specified
      if(Direction=="lower"){
        Data[Data<Threshold]<-9999
      }else{
        Data[Data>Threshold]<-9999
      }
      X<-rle(as.character(Data))
      X<-X$lengths[X$values==9999]
      if(length(X)>0){
        return(FUN(X))
      }else{
        return(0)
      }
    }

    Y<-do.call("cbind",lapply(1:nrow(Thresholds),FUN=function(i){
      Threshold<-Thresholds[i,Threshold]

      if(Thresholds[i,Direction]=="higher"){
        ColName<-paste0("Rain.G",Threshold)

        Z<-do.call("cbind",lapply(RSeqLen,FUN=function(j){
          ZDays(Rain,Threshold=Threshold,FUN=function(x)sum(x>j),Direction="higher")
        }))

        colnames(Z)<-paste0("N.Seq.D",RSeqLen)

        X<-data.table(
          Days=round(sum(Rain>Threshold),2),
          Days.Pr=round(sum(Rain>Threshold)/length(Rain),2),
          Max.Seq=ZDays(Rain,Threshold=Threshold,FUN=max,Direction="higher"),
          Z
        )

      }else{
        ColName<-paste0("Rain.L",Thresholds[i,Threshold])

        Z<-do.call("cbind",lapply(RSeqLen,FUN=function(j){
          ZDays(Rain,Threshold=Threshold,FUN=function(x)sum(x>j),Direction="lower")
        }))

        colnames(Z)<-paste0("N.Seq.D",RSeqLen)

        X<-data.table(
          Days=round(sum(Rain<Threshold),2),
          Days.Pr=round(sum(Rain<Threshold)/length(Rain),2),
          Max.Seq=ZDays(Rain,Threshold=Threshold,FUN=max,Direction="lower"),
          Z
        )
      }

      colnames(X)<-paste0(ColName,".",colnames(X))
      X

    }))

    X<-data.table(
      Rain.sum=sum(Rain),
      ETo.sum=sum(ETo),
      ETo.NA=sum(is.na(ETo)),
      WBalance=sum(Rain)-sum(ETo),
      WBalance.NegDays=sum((Rain-ETo)<0),
      Y
    )[is.na(ETo.sum),WBalance:=as.numeric(NA)]

    return(X[,lapply(.SD,as.numeric)])

  }

  ERatio.Calc<-function(ERatio,Thresholds,RSeqLen){

    ZDays<-function(Data,Threshold=0,FUN=max){
      # Direction = lower or higher than the threshold specified
      Data[Data<Threshold]<-9999

      X<-rle(as.character(Data))
      X<-X$lengths[X$values==9999]
      if(length(X)>0){
        return(FUN(X))
      }else{
        return(0)
      }
    }

    Y<-do.call("cbind",lapply(1:length(Thresholds),FUN=function(i){
      Threshold<-Thresholds[i]

      ColName<-paste0("ERatio.L",Thresholds[i])

      Z<-do.call("cbind",lapply(RSeqLen,FUN=function(j){
        ZDays(ERatio,Threshold=Threshold,FUN=function(x)sum(x>j))
      }))

      colnames(Z)<-paste0("N.Seq.D",RSeqLen)

      X<-data.table(
        Days=round(sum(ERatio<Threshold),2),
        Days.Pr=round(sum(ERatio<Threshold)/length(ERatio),2),
        Max.Seq=ZDays(ERatio,Threshold=Threshold,FUN=max),
        Z
      )


      colnames(X)<-paste0(ColName,".",colnames(X))
      X

    }))

    X<-data.table(
      ERatio.mean=mean(ERatio),
      ERatio.median=as.numeric(median(ERatio)),
      ERatio.min=min(ERatio),
      Y
    )

    return(X[,lapply(.SD,as.numeric)])

  }

  Logging.Calc<-function(Logging,ssat,RSeqLen){

    Thresholds<-c(0,ssat*0.5,ssat*0.9)
    TNames<-c(0,"ssat0.5","ssat0.9")

    ZDays<-function(Data,Threshold=0,FUN=max){
      # Direction = lower or higher than the threshold specified
      Data[Data>Threshold]<-9999

      X<-rle(as.character(Data))
      X<-X$lengths[X$values==9999]
      if(length(X)>0){
        return(FUN(X))
      }else{
        return(0)
      }
    }

    Y<-do.call("cbind",lapply(1:length(Thresholds),FUN=function(i){
      Threshold<-Thresholds[i]

      ColName<-paste0("Logging.G",TNames[i])

      Z<-do.call("cbind",lapply(RSeqLen,FUN=function(j){
        ZDays(Logging,Threshold=Threshold,FUN=function(x)sum(x>j))
      }))

      colnames(Z)<-paste0("N.Seq.D",RSeqLen)

      X<-data.table(
        Days=round(sum(Logging>Threshold),2),
        Days.Pr=round(sum(Logging>Threshold)/length(Logging),2),
        Max.Seq=ZDays(Logging,Threshold=Threshold,FUN=max),
        Z
      )

      colnames(X)<-paste0(ColName,".",colnames(X))
      X

    }))

    X<-data.table(
      Logging.sum=sum(Logging,na.rm=T),
      Logging.mean=mean(Logging,na.rm=T),
      Logging.median=median(Logging,na.rm=T),
      Logging.present.mean=if(length(Logging[Logging>0])>0){mean(Logging[Logging>0])}else{0},
      Y
    )

    return(X[,lapply(.SD,as.numeric)])

  }

  TEMP.Calc<-function(Tmax,Tmin,Tmean,Thresholds,TSeqLen){

    ZDays<-function(Data,Threshold=0,FUN=max,Direction="lower"){
      # Direction = lower or higher than the threshold specified
      if(Direction=="lower"){
        Data[Data<Threshold]<-9999
      }else{
        Data[Data>Threshold]<-9999
      }
      X<-rle(as.character(Data))
      X<-X$lengths[X$values==9999]
      if(length(X)>0){
        return(FUN(X))
      }else{
        return(0)
      }
    }

    Y<-do.call("cbind",lapply(1:nrow(Thresholds),FUN=function(i){
      Threshold<-Thresholds[i,Threshold]

      if(Thresholds[i,Direction]=="higher"){
        ColName<-paste0("Tmax.TG",Threshold)

        Z<-do.call("cbind",lapply(TSeqLen,FUN=function(j){
          ZDays(Tmax,Threshold=Threshold,FUN=function(x)sum(x>j),Direction="higher")
        }))

        colnames(Z)<-paste0("N.Seq.D",TSeqLen)

        X<-data.table(
          Days=round(sum(Tmax>Threshold),2),
          Days.Pr=round(sum(Tmax>Threshold)/length(Tmax),2),
          Max.RSeq=ZDays(Tmax,Threshold=Threshold,FUN=max,Direction="higher"),
          Z
        )

      }else{
        ColName<-paste0("Tmin.TL",Thresholds[i,Threshold])

        Z<-do.call("cbind",lapply(TSeqLen,FUN=function(j){
          ZDays(Tmin,Threshold=Threshold,FUN=function(x)sum(x>j),Direction="lower")
        }))

        colnames(Z)<-paste0("N.Seq.D",TSeqLen)

        X<-data.table(
          Days=round(sum(Tmin<Threshold)/length(Tmin),2),
          Max.RSeq=ZDays(Tmin,Threshold=Threshold,FUN=max,Direction="lower"),
          Z
        )
      }

      colnames(X)<-paste0(ColName,".",colnames(X))
      X

    }))

    X<-data.table(
      Y,
      Tmin.min=min(Tmin),
      Tmin.mean=mean(Tmin),
      Tmin.var=var(Tmin),
      Tmin.sd=sd(Tmin),
      Tmin.range=diff(range(Tmin)),

      Tmax.max=max(Tmax),
      Tmax.mean=mean(Tmax),
      Tmax.var=var(Tmax),
      Tmax.sd=sd(Tmax),
      Tmax.range=diff(range(Tmax)),

      Tmean.max=max(Tmean),
      Tmean.min=min(Tmean),
      Tmean.mean=mean(Tmean),
      Tmean.var=var(Tmean),
      Tmean.sd=sd(Tmean),
      Tmean.range=diff(range(Tmean))
    )


    return(X[,lapply(.SD,as.numeric)])


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
      sprintf("%03s",format(X,"%j"))
    }
  }

  # Create a text file detailing parameters used in analysis
  if(!is.na(SaveDir)){
    ParamSave<-list(Rain.Windows=c("Rain.Windows:",Rain.Windows),
                    Widths=c("Rain.Window.Widths:",Rain.Window.Widths),
                    Rain.Window.Threshold=c("Rain.Window.Threshold:",Rain.Window.Threshold),
                    Rain.Threshold=c("Rain.Threshold:",unlist(Rain.Threshold)),
                    RSeqLen = c("RSeqLen:",RSeqLen),
                    Temp.Threshold=c("Temp.Threshold:",unlist(Temp.Threshold)),
                    TSeqLen = c("TSeqLen:",TSeqLen),
                    Max.LT.Avg=c("Max.LT.Avg:",Max.LT.Avg),
                    PrePlantWindow=c("PrePlantWindow:",PrePlantWindow),
                    Windows=c("Windows:",unlist(Windows)),
                    Win.Start=c("Win.Stat: ", Win.Start),
                    Rain.Data.Name=c("Rain.Data.Name:",Rain.Data.Name),
                    Temp.Data.Name=c("Temp.Data.Name:",Temp.Data.Name),
                    Do.LT.Avg=c("Do.LT.Avg:", Do.LT.Avg),
                    Do.BioClim=c("Do.BioClim:",Do.BioClim)
    )

    if(file.exists(paste0(SaveDir,"/parameters.txt"))){
      unlink(paste0(SaveDir,"/parameters.txt"))
    }

    lapply(ParamSave, cat, "\n", file=paste0(SaveDir,"/parameters.txt"), append=TRUE)
  }

  CLIMATE<-data.table(CLIMATE)

  # Create unique Site x EU x Date combinations
  SS<-Data[Product!=""]

  # Check that SeasonLength.Data is not <10 days
  SLenError<-SS[SeasonLength.Data<=10]

  if(nrow(SLenError)>0){
    print(paste0("ERROR: ",SS[SeasonLength.Data<=10,paste(unique(Code),collapse = ", ")]," contains season lengths <10 days"))
    SS<-SS[!(SeasonLength.Data<=10 & !is.na(SeasonLength.Data))]
  }

  SaveName<-paste0(SaveDir,"/ClimStats.RData")



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

    # Check for any climate data with missing CHIRPS rainfall data
    MissingRain<-names(CLIMATE)[unlist(lapply(CLIMATE,FUN=function(X){X[,all(is.na(Rain))]}))]

    if(length(MissingRain>0)){
      print("Sites with no rainfall data:")
      print(Data[Site.Key %in% MissingRain,unique(Site.Key)])

      SS<-SS[!Site.Key %in% MissingRain]
    }

    # Create vector sites in dataset
    Sites<-as.vector(unlist(unique(SS[,..ID])))

    MCode.SS<-apply(SS[,c(..ID,"EU","PlantingDate","M.Year")],1,paste,collapse="_")

    # Determine years for which complete data is available (only to be used for annual calcs., e.g. BIOCLIM)
    Years<-table(CLIMATE[[1]]$Year)
    Years<-names(Years[Years>=365])

    # Could this be speeded up by running a parallel lapply on CLIMATE? The main issue is CLIMATE being loaded into RAM for each core.
    B<-lapply(Sites,FUN=function(Site){


      Message<-paste0(Temp.Data.Name," x ",Rain.Data.Name,": Estimating seasonal climate for  site: ",match(Site,Sites),"/",length(Sites))

      if(DebugMode){
        cat(paste(Message,"\n"))
      }else{
        cat('\r                                                                                                                                          ')
        cat('\r',Message)
        flush.console()
      }

      SS.N<-SS[which(SS[,..ID]==Site)][is.na(M.Season),M.Season:=""]

      Climate<-CLIMATE[[Site]]

      if(is.null(Climate)){
        "No site match in Climate Dataset"
      }else{

        A<-rbindlist(lapply(1:nrow(SS.N),FUN=function(i){

          WINS<-rbind(data.table(
            Name=c("Data","EcoCrop"),
            Start=rep(Win.Start,2),
            End=c(SS.N[i,SeasonLength.Data], SS.N[i,SeasonLength.EcoCrop])),
            Windows
          )

          Z<-lapply(1:nrow(WINS),FUN=function(j){
            if(!is.na(WINS[j,End])){
              C<-Climate[Climate$Date>=(SS.N[i,PlantingDate]+ WINS[j,Start])& Climate$Date<=(SS.N[i,PlantingDate] + WINS[j,End])]

              D<-Climate[Climate$Date>=(SS.N[i,PlantingDate]-PrePlantWindow) & Climate$Date<SS.N[i,PlantingDate]]

              # Make sure the climate dataset is complete
              if(!(nrow(C)<(WINS[j,End]-WINS[j,Start]) | sum(is.na(C$Rain)>0))){ # Technically should have a +1 on the left, here we are giving -1 day leeway

                C<-c(sapply(GDD(Tmax=C$Temp.Max,Tmin=C$Temp.Min,Tlow=SS.N[i,Tlow],Thigh=SS.N[i,Thigh],Topt.low = SS.N[i,Topt.low],Topt.high = SS.N[i,Topt.high],ROUND=2),sum),
                     unlist(RAIN.Calc(C$Rain,C$ETo,Thresholds=Rain.Threshold,RSeqLen=RSeqLen)),
                     unlist(TEMP.Calc(Tmax=C$Temp.Max, Tmin=C$Temp.Min, Tmean=C$Temp.Mean,Thresholds=Temp.Threshold,TSeqLen=TSeqLen)),
                     unlist(ERatio.Calc(C$ERATIO,Thresholds=ER.Threshold,RSeqLen=ERSeqLen)),
                     unlist(Logging.Calc(C$LOGGING,ssat=C$ssat[1],RSeqLen=LSeqLen)))
                suppressWarnings(C[[paste0("Rain.sum.PrePlant",PrePlantWindow)]]<-D[,sum(Rain)])
                suppressWarnings(C[[paste0("ETo.sum.PrePlant",PrePlantWindow)]]<-D[,sum(ETo)])
                suppressWarnings(C$EU<-SS.N$EU[i])
                C$PD.Used<-SS.N$PlantingDate[i]
                C$W.Start<-WINS$Start[j]
                C$W.End<-WINS$End[j]
                C$W.Name<-WINS$Name[j]
                C$ID<-Site
                C$M.Year<-SS.N$M.Year[i]
                C$Season<-SS.N$M.Season[i]
                as.data.frame(C)

              }else{
              }
            }else{

            }

          })

          rbindlist(Z[unlist(lapply(Z,FUN=function(X){!is.null(X)}))])


        }))

        if(Do.LT.Avg){

          EU.N.S<-unique(SS.N[,c("EU","M.Season")])

          LT.A<-lapply(1:nrow(EU.N.S),FUN=function(i){


            Message<-paste0(Temp.Data.Name," x ",Rain.Data.Name,": Estimating Long-term averages for  site: ",match(Site,Sites),"/",length(Sites)," | Season: ",EU.N.S$M.Season[i]," | EU: ",EU.N.S$EU[i])

            if(DebugMode){
              cat(paste(Message,"\n"))
            }else{
              cat('\r                                                                                                                                          ')
              cat('\r',Message)
              flush.console()
            }

            PDates<-as.Date(paste0(Year.Range,"-",SS.N[EU==EU.N.S$EU[i] & M.Season==EU.N.S$M.Season[i],C.Med(PlantingDate,FUN=stats::median)]),format="%Y-%j")

            # Excise temporal windows from climate data
            C1<-data.table(Start=which(Climate$Date %in% PDates)-Rain.Windows[1])[,End:=Start+sum(Rain.Windows)][!Start<=0][!End>nrow(Climate)]
            C<-Climate[unlist(lapply(1:nrow(C1),FUN=function(l){unlist(C1[l,1]):unlist(C1[l,2])})),][!is.na(Year)]

            C[,Sequence:=C1[,Nrow:=1:nrow(C1)][,rep(Nrow,length(Start:End)),by=Nrow][,V1]]

            # Remove incomplete sequences by cross referencing the width the window should be
            C<-C[Sequence %in%  C[,.N == sum(Rain.Windows)+1,by=Sequence][,Sequence]]

            # Add "data" year
            C[,YearX:=Year[1],by=Sequence]

            # Work out planting date for each sequence
            XX<-C[,Est.Rain(Rain,Date,..Rain.Window.Widths,..Rain.Window.Threshold,..Rain.Windows),by=list(YearX,Sequence)]

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
              PD.Date.Pub<-SS.N[EU==EU.N.S[i,EU] &  M.Season==EU.N.S[i,M.Season] & PlantingDate>=C[,min(Date)],PlantingDate]
              Annual.Plant[na.omit(match(format(PD.Date.Pub,"%Y"),P.Years))]<-PD.Date.Pub

              No.PDate.Flag<-"No seasons met rainfall threshold, mid-point of published planting period used."
              C.val<-circular::circular(as.numeric(format(as.Date(paste0("2000",substr(as.character(Annual.Plant[!is.na(Annual.Plant)]), 5, 10))),"%j"))*360/365*pi/180)
            }else{
              C.val<-circular::circular(as.numeric(format(as.Date(paste0("2000",substr(as.character(Annual.Plant[N1]), 5, 10))),"%j"))*360/365*pi/180)
            }

            LT.Mean<-as.numeric(round(mean(C.val,na.rm=T)*180/pi*365/360,ROUND))
            LT.Median<-as.numeric(round(median(C.val,na.rm=T)*180/pi*365/360,ROUND))

            if(LT.Mean<=0){
              LT.Mean<-365+LT.Mean
            }

            if(LT.Median<=0){
              LT.Median<-365+LT.Median
            }

            # Calculate LT.Avg Planting date standard deviation
            LT.SD<-round(sd(C.val,na.rm = T)*180/pi*365/360,ROUND)

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
                                                            Season=EU.N.S$M.Season[i],
                                                            ID=Site,
                                                            P.Data.Flag=No.PDate.Flag),
                                LT.Averages=data.table(Mean=LT.Mean,
                                                       Median=LT.Median,
                                                       SD=LT.SD,N=NYear.LT.Avg,
                                                       EU=EU.N.S$EU[i],
                                                       Season=EU.N.S$M.Season[i],
                                                       ID=Site,
                                                       P.Data.Flag=No.PDate.Flag))

            # Now Estimate Temperature and Rainfall LTAvgs using the average season lengths and pre-set windows
            WINS<-rbind(data.table(
              Name=c("Data","EcoCrop"),
              Start=rep(Win.Start,2),
              End=c(SS.N[i,SeasonLength.Data][1], SS.N[i,SeasonLength.EcoCrop][1])),
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
                  C[,RAIN.Calc(Rain,ETo,Thresholds=Rain.Threshold,RSeqLen=RSeqLen),by=c("P.Year","H.Year")][,-c(1:2)],
                  C[,TEMP.Calc(Tmax=Temp.Max, Tmin=Temp.Min, Tmean=Temp.Mean,Thresholds=Temp.Threshold,TSeqLen=TSeqLen),by=c("P.Year","H.Year")][,-c(1:2)],
                  C[,ERatio.Calc(ERATIO,Thresholds=ER.Threshold,RSeqLen=ERSeqLen),by=c("P.Year","H.Year")][,-c(1:2)],
                  C[,Logging.Calc(LOGGING,ssat=ssat[1],RSeqLen=LSeqLen),by=c("P.Year","H.Year")][,-c(1:2)]
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
                LT.Avg$Season<-EU.N.S$M.Season[i]
                LT.Avg$ID<-Site
                LT.Avg$W.Start<-WINS[k,Start]
                LT.Avg$W.End<-WINS[k,End]
                LT.Avg$W.Name<-WINS[k,Name]
                LT.Avg$P.Data.Flag<-No.PDate.Flag

                Annual.Estimates$EU<-EU.N.S$EU[i]
                Annual.Estimates$Season<-EU.N.S$M.Season[i]
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

          names(LT.A)<-apply(unique(SS.N[,c("EU","Product","M.Season")]),1,paste,collapse="-")

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

    MCode<-unique(apply(Seasonal.Clim[,c("ID","EU","PD.Used","M.Year")],1,paste,collapse="_"))
    MCode<-MCode.SS[!MCode.SS %in% MCode]

    if(length(MCode)>0){
      fwrite(data.table(Missing=unique(MCode)),paste0(SaveDir,"/MissingLines.csv"))
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

      Seasonal<-list(Observed=Seasonal.Clim,LongTerm=list(LT.PD.Years=LT.PD.Years,LT.PD.Avg=LT.PD.Avg,LT.Clim.Years=LT.Clim.Years,LT.Clim.Avg=LT.Clim.Avg))

    }else{
      Seasonal<-list(Observed=Seasonal.Clim)
    }


    if(!is.na(SaveDir)){
      save(Seasonal,file=SaveName)
    }


  if(Do.BioClim){

    SaveName<-paste0(SaveDir,"/BioClim.RData")

    Message<-paste0(Temp.Data.Name," x ",Rain.Data.Name,": Calculating BioClim Annual Variables")

    if(DebugMode){
      cat(paste(Message,"\n"))
    }else{
      cat('\r                                                                                                                                          ')
      cat('\r',Message)
      flush.console()
    }

    CLIMATE1<-CLIMATE1[Year %in% Years,]
    colnames(CLIMATE1)[colnames(CLIMATE1)==ID]<-"ID"

    BIOV<-CLIMATE1[,round(dismo::biovars(Rain,Temp.Min,Temp.Max),2),by=c("Year","ID")][,Variable:= rep(paste("BIO",1:19),.N/19)]
    BIOV<-dcast(BIOV,Year+ID ~ Variable,value.var="V1")

    BIOV.LT<-BIOV[Year<=Max.LT.Avg,lapply(.SD,FUN=function(X){round(c(mean(X),median(X),sd(X)),2)}),by="ID",.SDcol=3:ncol(BIOV)
    ][,Variable:=rep(c("Mean","Median","SD"),.N/3)][,N:=sum(Years<=Max.LT.Avg)]

    Annual.Estimates<-rbind(
      cbind(BIOV[,1:2],(BIOV[,3:21]-BIOV.LT[Variable=="Mean",][match(BIOV[,ID],BIOV.LT[Variable=="Mean",ID]),-c(1,21,22)])[,Variable:="Dev.Mean"]),
      cbind(BIOV[,1:2],(BIOV[,3:21]-BIOV.LT[Variable=="Median",][match(BIOV[,ID],BIOV.LT[Variable=="Median",ID]),-c(1,21,22)])[,Variable:="Dev.Mean"]),
      BIOV[,Variable:="Annual Value"]
    )


    BIOV<-list(Annual.Estimates=Annual.Estimates,LT.Averages = BIOV.LT)

    if(!is.na(SaveDir)){
      save(BIOV,file=SaveName)
    }

    Seasonal$BioClim<-BIOV

  }

  # Save Parameters
  ParamSave<-list(Rain.Windows=Rain.Windows,
                  Widths=Rain.Window.Widths,
                  Rain.Window.Threshold=Rain.Window.Threshold,
                  Rain.Threshold=Rain.Threshold,
                  RSeqLen=RSeqLen,
                  Temp.Threshold=Temp.Threshold,
                  TSeqLen=TSeqLen,
                  Max.LT.Avg=Max.LT.Avg,
                  PrePlantWindow=PrePlantWindow,
                  Windows=Windows,
                  Win.Start=Win.Start,
                  Rain.Data.Name=Rain.Data.Name,
                  Temp.Data.Name=Temp.Data.Name)

  if(!is.na(SaveDir)){
    save(ParamSave,file=paste0(SaveDir,"/parameters.RData"))
  }
  Seasonal$Parameters<-ParamSave

  # Save Dataset Used
  if(!is.na(SaveDir)){
    save(Data,file=paste0(SaveDir,"/Data.RData"))
  }

  Seasonal$Data<-Data


  return(Seasonal)

}


