#' Estimate uncertain planting dates from rainfall data
#'
#' This function can be used to estimate planting datesfrom daily rainfall where there is uncertainty as calculated by substracting `Plant.Start` from `Plant.End`.
#'
#' A daily rainfall dataset extracted for the data supplied in the `Data` argument needs to be supplied to this function, we recommend using
#' the `ExtractCHIRPS` function to generate rainfall data. In the `Rain.Data.Name` argument please supply the name of the rainfall dataset (e.g. `CHIRPS`).
#' Use the `Rain.Field` argument to specify the name of the column containing the rainfall amount. Matching between `Data` and `Rain.Data` uses the
#' `Daycount` and a location identity field as named using the `ID` argument, as such the datasets must share the same point of origin for calculation
#' of `Daycount` and the same name for the `ID` field.
#'
#' Planting dates will estimated for rows in `Data` where the difference between `Plant.Start` and `Plant.End` is greater than or equal to the
#' `Uncertainty.Min` argument and less than or equal to the `Uncertainty.Max` argument.
#'
#' `EstPDayRain` uses the  \link[zoo]{rollapply} function to sum rainfall within a scanning window, planting is assumed to occur the day **after**
#' summed rainfall surpasses a threshold amount.
#'
#' For each row of `Data` with appropriate planting uncertainty the function initially searches for rainfall events in `Rain.Data` in-between and including the corresponding `Plant.Start` and `Plant.End` dates.
#' This temporal search period can be modified using the `DaysBefore` and `MultiplyWin` arguments.`DaysBefore` extends the `Plant.Start` date backwards by a number of days.
#' `MultiplyWin` is a proportion that multiplies the difference between `Plant.Start` and `Plant.End` dates increasing the size of the period forwards in time.
#' If `Plant.Start = 2018-01-01` and `Plant.End = 2018-01-11` the difference between them is 10 days, if `MultiplyWin = 1.5` then `10*1.5=15` and the
#' end of the initial search window becomes `2018-01-01 + 15 = 2018-01-16`. The width (in days) of the `rollapply` scanning window is specified
#' by the `Width`argument and the amount of rainfall (mm) required to trigger planting within the scanning window is specified by the `Rain.Thresholds` argument.
#'
#' Up to two additional temporal search periods after the above can be specified in days using the `Window` arguments, for each extra window added `Width` and
#' `Rain.Thresholds` arguments require values to added in sequence. If no trigger events are found in the initial scanning window then subsequent windows
#' are searched accordingly.
#
#' @param Data An ERA dataset (e.g. `ERA.Compiled`)
#' @param ID A character vector of length one containing the column name for a unique id field naming each location in the dataset provided.
#' @param SaveDir A character vector of length one containing the path to the directory where the extracted data are to be saved. Set to NA if you do not want to save the returned dataset.
#' @param Rain.Data Data.frame or data.table of daily rainfall with a field of class `Date` named `Date`, a numeric field named as per the `Rain.Field` argu containing daily rainfall amount in mm and ID field matching Data object supplied.
#' @param Rain.Data.Name A character vector of length one containing the name of the rainfall dataset (e.g. "CHIRPS")
#' @param Rain.Field A character vector of length one containing the name of the rainfall field in the `Rain.Data` object supplied (e.g. `Rain`)
#' @param DaysBefore An integer vector of length one; when searching for rainfall events in-between the planting start/end dates specified, extend the planting start date backwards by a number of days
#' @param MultiplyWin An numeric vector of length one; this parameter is a proportion that changes the size of the difference between plant start and plant end, 1= no change
#' @param Window An integer vector of max length two which specifies in days up to two addition temporal periods beyond the initial temporal such period. Can be set to NA if no additional temporal search periods are required.
#' @param Widths An integer vector of length equivalent to 1 + length(`Window`), max length = 3; the width of the scanning window in days within which rainfall is summed for the initial window of planting uncertainty plus each of the additional periods specified in the `Windows` argument.
#' @param Rain.Thresholds An integer vector of length equivalent to 1 + length(`Window`), max length = 3; the amount of rainfall that has to fall in the temporal windows considered.
#' @param Uncertainty.Min An integer vector of length one; the minimum acceptable difference between plant.start and plant.end in days
#' @param Uncertainty.Max An integer vector of length one; the maximum acceptable difference between plant.start and plant.end in days
#' @param Add.Values Logical `T/F`; if `T` append results to the Data object supplied and return, else return a separate table.
#' @param Use.Data.Dates Logical `T/F`; only relevant if planting date estimates have been added to the Data object using the `ERAg::Est.PDay.Data` function. If `T` then NA values in for Plant.Start and Plant.End are replaced by values estimated using the Est.PDay.Data function.
#' @return The `EstPDayRain` function generates a field of class `Date` named `paste0("UnC.",Rain.Data.Name,".P.Date")` containing planting date estimates
#' for rows of `Data` where planting uncertainty is within the range specfied by the `Uncertainty.Min` and `Uncertainty.Max` arguments. If the `Add.Values`
#' arguments is `TRUE` then this field is appended to `Data` else it is returned in a separate `data.table.`
#' @export
#' @import data.table
#' @importFrom zoo rollapply as.zoo
EstPDayRain<-function(Data,
ID,
Rain.Data,
Rain.Data.Name,
Rain.Field,
DaysBefore = 0,
MultiplyWin = 1,
Window = c(14,14),
Widths = c(2,3,3),
Rain.Thresholds,
Uncertainty.Min = 7,
Uncertainty.Max = 90,
Add.Values = T,
Use.Data.Dates = F
){

  Data<-data.table(Data)
  Rain.Data<-data.table(Rain.Data)

  RAIN<-split(Rain.Data,by=ID)

  if(Use.Data.Dates==T){
    Data[is.na(Plant.Start),Plant.Start:=as.Date(Data.PS.Date)][is.na(Plant.End),Plant.End:=as.Date(Data.PE.Date)]
  }

  # Convert dates to correct format and work out planting window and average dates
  Data[,Plant.Start:=if(class(Plant.Start)=="Date"){Plant.Start}else{as.Date(Plant.Start,"%d.%m.%Y")}
  ][,Plant.End:=if(class(Plant.End)=="Date"){Plant.End}else{as.Date(Plant.End,"%d.%m.%Y")}
  ][,Harvest.Start:=if(class(Harvest.Start)=="Date"){Harvest.Start}else{as.Date(Harvest.Start,"%d.%m.%Y")}
  ][,Harvest.End:=if(class(Harvest.End)=="Date"){Harvest.End}else{as.Date(Harvest.End,"%d.%m.%Y")}
  ][,Plant.Diff:=as.numeric(Plant.End-Plant.Start)
  ][,Plant.Avg:=Plant.Start+Plant.Diff/2
  ][,Harvest.Diff:=as.numeric(Harvest.End-Harvest.Start)
  ][,Harvest.Avg:=Harvest.Start+Harvest.Diff/2]


  #Make metadata list for analysis parameters
  META<-list(Window=Window,Uncertainty.Min=Uncertainty.Min,Uncertainty.Max=Uncertainty.Max,Widths=Widths,Rain.Thresholds=Rain.Thresholds,DaysBefore=DaysBefore)


  SS<-unique(Data[Plant.Diff>=Uncertainty.Min & Plant.Diff<=Uncertainty.Max,c(..ID,"Plant.Start","Plant.End","Plant.Diff","Plant.Avg")])
  colnames(SS)[colnames(SS)==ID]<-"ID"

  Sites<-SS[,unique(ID)]

  # Check for non-matches between Sites and Rainfall Data
  Data$ID<-Data[,..ID]
  Missing.Sites<-unique(Data[ID %in% unique(SS$ID[which(!SS$ID %in% names(RAIN))]),c(..ID,"Plant.Diff")])

  RAIN<-RAIN[names(RAIN) %in% Sites]
  Sites<-Sites[Sites %in% names(RAIN)]

  if(nrow(Missing.Sites)>0){
    print("Some sites are missing rainfall data")
    View(Missing.Sites)
    Missing.Sites<-unlist(unique(Missing.Sites[,..ID]))
    # Remove non-matches from SS & Sites
    SS<-SS[!ID %in% Missing.Sites]
    Sites<-Sites[!Sites %in% Missing.Sites]
  }


  cat('\r                                                                                                                                          ')
  cat('\r',"Estimating Planting Statistics for Sites")
  flush.console()

  B<-rbindlist(pblapply(1:length(Sites),FUN=function(i){


    Rain<-RAIN[[Sites[i]]]
    X<-SS[ID==Sites[i]]

    Z<-rbindlist(lapply(1:nrow(X),FUN=function(j){

      Y<-X[(j),Plant.Start]
      Diff<-X[(j),Plant.Diff]
      R<-which(zoo::rollapply(zoo::as.zoo(unlist(Rain[Date>=(Y-DaysBefore) & Date<=Y+Diff*MultiplyWin,..Rain.Field])),width=Widths[1],sum)>Rain.Thresholds[1])

      if(length(R)>0){
        R<-Y+R[1]
      }else{
        if(!is.na(Window[1])){
          R<-which(zoo::rollapply(zoo::as.zoo(unlist(Rain[Date>=Y+Diff*MultiplyWin+1 & Date<=Y+Diff*MultiplyWin+Window[1],..Rain.Field])),width=Widths[2],sum)>Rain.Thresholds[2])
        }
        if(length(R)>0){
          R<-Y+R[1]
        }else{
          if(!is.na(Window[2])){
            R<-which(zoo::rollapply(zoo::as.zoo(unlist(Rain[Date>=Y+Diff*MultiplyWin+Window[1]+1 & Date<=Y+Diff*MultiplyWin+Window[1]+Window[2],..Rain.Field])),width=Widths[3],sum)>Rain.Thresholds[3])
          }
          if(length(R)>0){
            R<-Y+R[1]
          }else{
            R<-as.Date(NA)
          }}}


      data.table(Rain.Start.End=sum(Rain[Date>=Y & Date<=Y+Diff,..Rain.Field]),Est.PDate=R)

    }))

    cbind(X,Z)

  }))


  setnames(B,"Est.PDate",paste0("UnC.",Rain.Data.Name,".P.Date"))


  if(Add.Values){

    cat('\r                                                                                                                                          ')
    cat('\r',"Matching Planting Stats to Data")
    flush.console()

    N<-!colnames(Data) %in% paste0("UnC.",Rain.Data.Name,"P.Date")
    Data<-Data[,..N]

    Data[,R:=1:nrow(Data)]
    Data$SID<-Data[,..ID]
    Data[,MCode:=paste(c(SID, Plant.Start,Plant.End),collapse="-"),by=list(SID, Plant.Start,Plant.End)]

    A<-B
    A[,R:=1:nrow(A)]
    A[,MCode:=paste(c(ID,Plant.Start,Plant.End),collapse="-"),by=R]
    Data<-cbind(Data,A[match(Data[,MCode],A[,MCode]),-c("ID","Plant.Start","Plant.End","Plant.Diff","Plant.Avg","R","MCode")])

    Data<-Data[,!c("R","SID","MCode","Plant.Diff","Harvest.Diff","Plant.Avg","Harvest.Avg")]

    return(Data)
  }else{
    return(B)
  }


}
