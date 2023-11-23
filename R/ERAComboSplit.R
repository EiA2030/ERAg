#' Split practice & product combinations making the dataset long
#'
#' `ERAComboSplit` takes observations of combined ERA practices or products (e.g. Inorganic Fertilizer-Reduced Tillage) and splits them into a longer dataset.
#' Each row in the dataset corresponds to a single instance of a practice or product in an observation (where that practice or product was initially combined with
#' others or alone).
#'
#' `ERAComboSplit` takes observations of a combined ERA practices or products (e.g. Inorganic Fertilizer-Reduced Tillage) splits them then duplicates the
#' observation for the number of practice x product combinations. Values in the related practice and product  name and code columns are renamed
#'  to one of the single practice x single product combinations.
#'
#' @param Data the `ERA.Compiled` dataset.
#' @return `ERAComboSplit` returns a longer form version of `ERA.Compiled` with suffixed `.Combo` columns appended indicating a single practice x product
#' combination in duplicate rows of the split dataset.
#' @export
#' @import data.table
ERAComboSplit<-function(Data){

  X<-strsplit(unlist(Data[,SubPrName]),"-")
  Len<-rep(1:nrow(Data),unlist(lapply(X,length)))
  Data<-Data[Len]
  Data[,SubPrName.Combo:=unlist(X)]

  PracticeCodes<-data.table(ERAg::PracticeCodes)

  Data[,PrName.Combo:=PracticeCodes[match(Data[,SubPrName.Combo],Subpractice.S),Practice]]

  Data[,Theme.Combo:=PracticeCodes[match(Data[,SubPrName.Combo],Subpractice.S),Theme]]


  X<-strsplit(Data[,Product.Simple],"-")
  Len<-rep(1:nrow(Data),unlist(lapply(X,length)))
  Data<-Data[Len]
  Data[,Product.Simple.Combo:=unlist(X)]

  EUCodes<-data.table(EUCodes)

  Data[,Product.Subtype.Combo:=EUCodes[match(Data[,Product.Simple.Combo],Product.Simple),Product.Subtype]]

  Data[,Product.Type.Combo:=EUCodes[match(Data[,Product.Simple.Combo],Product.Simple),Product.Type]]

  return(Data)

}
