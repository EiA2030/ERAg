#' derive_outcome_ratio
#' Calculates new outcomes by dividing one outcome from another. If function returns NULL then no derived data are present.
#' @param Data An ERA data.table (e.g. `ERAg::ERA.Compiled`).
#' @param target ERA outcome code of target outcome (see `ERAg::OutcomeCodes`)
#' @param subtract ERA outcome code of outcome that will be substracted from the target outcome (see `ERAg::OutcomeCodes`)
#' @param result ERA outcome code for the resulting values (see `ERAg::OutcomeCodes`)
#' @export
#' @import data.table
derive_outcome_ratio<-function(Data,numerator,denominator,result){

  OutcomeCodes<-data.table(ERAg::OutcomeCodes)

  X<-data.table::copy(Data)
  # Add ID field
  X[,IDx:=paste(TID,CID,EU,SubPrName,SubPrName.Base,Code,M.Year,Site.ID,Variety,Tree,Diversity,Duration,EU,Units)]

  # Remove groups that already have BCR outcome present
  X<-X[,BCR.Present:=any(Outcode %in% result),by=IDx][BCR.Present!=T]

  X<-X[Outcode %in% c(numerator,denominator)][,BothPresent:=length(unique(Outcode))==2,by=IDx]
  X<-X[BothPresent==T]
  if(nrow(X)==0){
    NULL
  }else{
    X[,N:=.N,by=IDx]

    # There should only be 2 practices, differences could be due to data being presented in two different places
    # In any case it probably indicates an error that should be investigated
    More.Than.2.SubPrNames<-rbind(
      X[N>2,list(T.Descrip,TID,C.Descrip,CID,Code,M.Year,MeanT,MeanC,Outcode,DataLoc,EU,Units),IDx][order(IDx)][Outcode==denominator],
      X[N>2,list(T.Descrip,TID,C.Descrip,CID,Code,M.Year,MeanT,MeanC,Outcode,DataLoc,EU,Units),IDx][order(IDx)][Outcode==numerator]
    )[,!"IDx"]

    # Once checking data for those papers with >2 add the dataloc to the IDx field
    X[N>2,IDx:=paste(IDx,DataLoc)]

    # Check cost and return are still present
    X<-X[,BothPresent:=length(unique(Outcode))==2,by=IDx]
    X<-X[BothPresent==T]

    X[,N:=.N,by=IDx]

    # Has adding dataloc "resolved" issue?
    Error1.Unresolved.by.Dataloc<-rbind(
      X[N!=2,list(T.Descrip,C.Descrip,Code,M.Year,MeanT,MeanC,Outcode,DataLoc),IDx][order(IDx)][Outcode==denominator],
      X[N!=2,list(T.Descrip,C.Descrip,Code,M.Year,MeanT,MeanC,Outcode,DataLoc),IDx][order(IDx)][Outcode==numerator]
    )[,!"IDx"]

    # Remove problem observations
    X<-X[N==2]

    if(nrow(X)>0){
      Y<-rbindlist(lapply(X[,unique(IDx)],FUN=function(CODE){
        Z<-X[IDx==CODE,list(Outcode,MeanT,MeanC)]
        Z<-data.table(
          IDx=CODE,
          MeanT=Z[Outcode==numerator,MeanT]/Z[Outcode==denominator,MeanT],
          MeanC=Z[Outcode==numerator,MeanC]/Z[Outcode==denominator,MeanC]
        )
        Z
      }))

      X<-X[Outcode!=numerator]

      X[,Outcode:=result]

      X<-X[,!c("MeanC","MeanT")]

      X<-cbind(X,Y[match(IDx,X[,IDx])])
      X<-X[,!c("IDx","N","BCR.Present","BothPresent")]

      return(list(data=X,errors1=More.Than.2.SubPrNames,errors2=Error1.Unresolved.by.Dataloc))
    }
  }

}
