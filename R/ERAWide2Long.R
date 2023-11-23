#' Convert ERA from wide to long format
#'
#' This function takes wide format ERA dataset where a control is compared to an experimental treatment and converts it to a long format where each row is a single unique outcome observation.
#'
#' @param Data The ERA.Compiled dataset or a subset of it.
#' @return A data.table where control vs experimental treatment comparisons have been removed. Each unique outcome observation (control or experimental) is a row in this table.
#' @export
ERAWide2Long<-function(Data){


  Data[,Index:=1:.N
  ][Tree=="",Tree:=NA
  ][Diversity=="",Diversity:=NA
  ][Variety=="",Variety:=NA]


  # Need to create control "Variety","Diversity","Tree" columns if codes for relevant practices match in T vs C cols

  C.Cols<-c("CID","C.Descrip",paste0("C",1:13),"C.NO","C.NI","MeanC","C.Feed.Source","USD2010.C")
  T.Cols<-c("TID","T.Descrip",paste0("T",1:13),"T.NO","T.NI","MeanT","T.Feed.Source","USD2010.T","Variety","Diversity","Tree")

  Remove<-c("yi","pc","plist","base.list","SubPrName","PrName","Theme","SubPrName.Base","PrName.Base","Theme.Base")

  Data<-Data[,!..Remove]

  Shared<-colnames(Data)[!colnames(Data) %in% c(C.Cols,T.Cols)]

  T.Comb<-c(T.Cols,Shared)
  Treat<-Data[,..T.Comb]

  C.Comb<-c(C.Cols,Shared)
  Control<-Data[,..C.Comb]

  setnames(Control,c("C.NO","C.NI","MeanC","C.Feed.Source","USD2010.C","CID",paste0("C",1:13),"C.Descrip"),
           c("T.NO","T.NI","MeanT","T.Feed.Source","USD2010.T","TID",paste0("T",1:13),"T.Descrip"))

  # Is same improved variety in control & treatment?
  PracCodes<-data.table(ERAg::PracticeCodes)[Practice=="Improved Varieties",Code]

  MatchCodes<-function(Data,PracCodes){
    T.Codes<-paste0("T",1:13)
    C.Codes<-paste0("C",1:13)

    All.Codes<-c(T.Codes,C.Codes)

    suppressWarnings(Data[,All.Code:=as.numeric(factor(apply(Data[,..All.Codes],1,paste,collapse="-")))])

    T.Codes<-c(T.Codes,"All.Code")
    C.Codes<-c(C.Codes,"All.Code")
    All.Codes<-c(All.Codes,"All.Code")

    Data2<-unique(Data[,..All.Codes])

    TPracs<-apply(Data2[,..T.Codes],1,FUN=function(X){
      X<-X[X %in% PracCodes]
      if(length(X)==0){"NA"}else{X}
    })
    CPracs<-apply(Data2[,..C.Codes],1,FUN=function(X){
      X<-X[X %in% PracCodes]
      if(length(X)==0){"NA"}else{X}
    })

    Data2[,MatchStatus:=unlist(lapply(1:length(TPracs),FUN=function(i){
      all(TPracs[[i]] %in% CPracs[[i]]) & all(CPracs[[i]] %in% TPracs[[i]])
    }))]

    Match<-Data2[match(Data[,All.Code],All.Code),MatchStatus]
    return(Match)
  }

  VarMatch<-MatchCodes(Data=Data,
                       PracCodes=data.table(ERAg::PracticeCodes)[Practice=="Improved Varieties",Code])

  Control[,Variety:=as.character(NA)]
  Control[VarMatch,Variety:=Data[VarMatch,Variety]]

  # Is same diversification in control & treatment?

  PracCodes<-data.table(ERAg::PracticeCodes)[Theme=="Agroforestry"|
                                               Practice %in% c("Green Manure","Improved Fallow","Crop Rotation",
                                                               "Intercropping","Intercropping or Rotation"),Code]

  DivMatch<-MatchCodes(Data=Data,
                       PracCodes=PracCodes)

  Control[,Diversity:=as.character(NA)]
  Control[DivMatch,Diversity:=Data[DivMatch,Diversity]]

  # Is same tree the same in control & treatment?
  PracCodes<-data.table(PracticeCodes)[Theme=="Agroforestry",Code]

  TreeMatch<-MatchCodes(Data=Data,
                        PracCodes=PracCodes)

  Control[,Tree:=as.character(NA)]
  Control[TreeMatch,Tree:=Data[TreeMatch,Tree]]

  # Subset to unique values and join
  colnames(Control)[!colnames(Control) %in% colnames(Treat)]
  colnames(Treat)[!colnames(Treat) %in% colnames(Control)]

  Treat[,ID:=NULL][,Index:=NULL][,TorC:="T"]
  Control[,ID:=NULL][,Index:=NULL][,TorC:="C"]

  Long<-rbind(Treat,Control,use.names=T)

  return(Long)
}
