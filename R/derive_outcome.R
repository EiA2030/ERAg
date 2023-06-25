#' derive_outcome
#' Calculates new outcomes by dividing or subtracting one outcome from another. If function returns NULL then no derived data are present. Any observation outcome sets that already have a result outcome associated with them are removed.
#' @param Data An ERA data.table (e.g. `ERAg::ERA.Compiled`).
#' @param outcome_col
#' @param outcome1
#' @param outcome2
#' @param outcome_result
#' @param operation
#' @param match_fields
#' @param val_col1
#' @param val_col2
#' @export
#' @import data.table
derive_outcome<-function(Data,outcome_col="Out.SubInd",outcome1,outcome2,outcome_result,operation="subtract",match_fields,val_col1="MeanT",val_col2="MeanC"){

  X<-data.table::copy(Data)

  # Add ID field
  X[,N:=1:.N][,IDx:=paste(.SD,collapse="-"),.SDcols=match_fields,by=N]

  setnames(X,c(val_col1,val_col2,outcome_col),c("MeanC","MeanT","Outcode"))


  # Remove groups that already have target outcome present
  X<-X[,Out.Present:=any(Outcode %in% outcome_result),by=IDx][Out.Present!=T]

  # Are both outcomes present for a unique ID?
  X<-X[Outcode %in% c(outcome1,outcome2)][,BothPresent:=length(unique(Outcode))==2,by=IDx]
  X<-X[BothPresent==T]

  if(nrow(X)==0){
    NULL
  }else{

    # Count number of rows for each unique ID
    X[,N:=.N,by=IDx]

    # There should only be 2 rows
    match_fields2<-c("N",match_fields,"Outcode","IDx")

    More.Than.2.SubPrNames<-rbind(
      X[N>2,..match_fields2][Outcode %in% c(outcome1,outcome2)]
    )

    if(nrow(More.Than.2.SubPrNames)>0){
      print("Issue - more than 2 matching rows for a unique ID")
      print(More.Than.2.SubPrNames)
    }

    # Remove any data without 2 rows
    X<-X[N==2]

    Y<-rbindlist(lapply(X[,unique(IDx)],FUN=function(CODE){
      Z<-X[IDx==CODE,list(Outcode,MeanT,MeanC)]
      Z<-data.table(
        IDx=CODE,
        MeanT=if(operation=="subtract"){Z[Outcode==outcome1,MeanT]-Z[Outcode==outcome2,MeanC]}else{Z[Outcode==outcome1,MeanT]/Z[Outcode==outcome2,MeanC]},
        MeanC=if(operation=="subtract"){Z[Outcode==outcome1,MeanC]-Z[Outcode==outcome2,MeanC]}else{Z[Outcode==outcome1,MeanT]/Z[Outcode==outcome2,MeanC]}
      )
      Z
    }))

    # Edit input data table
    X<-X[Outcode!=outcome2]
    X[,Outcode:=outcome_result]

    # Remove value columns
    X<-X[,!c("MeanC","MeanT")]

    # Match in results
    X<-cbind(X,Y[match(IDx,X[,IDx])])
    X<-X[,!c("IDx","N","Out.Present","BothPresent")]

    # Remove any infinite values
    X<-X[!(is.infinite(MeanC)|is.infinite(MeanT))]

    setnames(X,c("MeanC","MeanT","Outcode"),c(val_col1,val_col2,outcome_col))

    return(list(data=X,errors1=More.Than.2.SubPrNames))
  }

}
