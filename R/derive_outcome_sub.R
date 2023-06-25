#' derive_outcome_sub
#' Calculates new outcomes by subtracting one outcome from another. If function returns NULL then no derived data are present. Any observation outcome sets that already have a result outcome associated with them are removed.
#' @param Data An ERA data.table (e.g. `ERAg::ERA.Compiled`).
#' @param target ERA outcome code of target outcome (see `ERAg::OutcomeCodes`)
#' @param subtract ERA outcome code of outcome that will be substracted from the target outcome (see `ERAg::OutcomeCodes`)
#' @param result ERA outcome code for the resulting values (see `ERAg::OutcomeCodes`)
#' @export
#' @import data.table
derive_outcome_sub<-function(Data,target=120,subtract=150,result=124){c("")

  OutcomeCodes<-data.table(ERAg::OutcomeCodes)

  X<-data.table::copy(Data)
  # Add ID field
  X[,IDx:=paste(TID,CID,EU,SubPrName,SubPrName.Base,Code,M.Year,Site.ID,Diversity,Variety,Tree,Duration,EU,Units)]

  # Remove groups that already have BCR outcome present
  X<-X[,Out.Present:=any(Outcode %in% result),by=IDx][Out.Present!=T]

  X<-X[Outcode %in% c(target,subtract)][,BothPresent:=length(unique(Outcode))==2,by=IDx]
  X<-X[BothPresent==T]
  if(nrow(X)==0){
    NULL
  }else{
    X[,N:=.N,by=IDx]

    # There should only be 2 practices, differences could be due to data being presented in two different places
    # In any case it probably indicates an error that should be investigated
    More.Than.2.SubPrNames<-rbind(
      X[N>2,list(T.Descrip,TID,C.Descrip,CID,Code,M.Year,MeanT,MeanC,Outcode,DataLoc,EU),IDx][order(IDx)][Outcode==target],
      X[N>2,list(T.Descrip,TID,C.Descrip,CID,Code,M.Year,MeanT,MeanC,Outcode,DataLoc,EU),IDx][order(IDx)][Outcode==subtract]
    )[,!"IDx"]

    # Once checking data for those papers with >2 add the dataloc to the IDx field
    X[N>2,IDx:=paste(IDx,DataLoc)]

    # Check cost and return are still present
    X<-X[,BothPresent:=length(unique(Outcode))==2,by=IDx]
    X<-X[BothPresent==T]

    X[,N:=.N,by=IDx]

    # Has adding dataloc "resolved" issue?
    Error1.Unresolved.by.Dataloc<-rbind(
      X[N!=2,list(T.Descrip,C.Descrip,Code,M.Year,MeanT,MeanC,Outcode,DataLoc),IDx][order(IDx)][Outcode==target],
      X[N!=2,list(T.Descrip,C.Descrip,Code,M.Year,MeanT,MeanC,Outcode,DataLoc),IDx][order(IDx)][Outcode==subtract]
    )[,!"IDx"]

    # Remove problem observations
    X<-X[N==2]

    Y<-rbindlist(lapply(X[,unique(IDx)],FUN=function(CODE){
      Z<-X[IDx==CODE,list(Outcode,MeanT,MeanC)]
      Z<-data.table(
        IDx=CODE,
        MeanT=Z[Outcode==target,MeanT]-Z[Outcode==subtract,MeanT],
        MeanC=Z[Outcode==target,MeanC]-Z[Outcode==subtract,MeanC]
      )
      Z
    }))

    X<-X[Outcode!=subtract]

    X[,Outcode:=result]

    X<-X[,!c("MeanC","MeanT")]

    X<-cbind(X,Y[match(IDx,X[,IDx])])
    X<-X[,!c("IDx","N","Out.Present","BothPresent")]

    return(list(data=X,errors1=More.Than.2.SubPrNames,errors2=Error1.Unresolved.by.Dataloc))
  }

}
