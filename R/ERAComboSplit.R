#' Split practice & product combinations making the dataset long
#'
#' `ERAComboSplit` takes observations of combined ERA practices or products (e.g. Inorganic Fertilizer-Reduced Tillage) and splits them into a longer dataset.
#' Each row in the dataset corresponds to a single instance of a practice or product in an observation (where that practice or product was intially combined with
#' others or alone).
#'
#' `ERAComboSplit` takes an observations of a combined ERA practices or products (e.g. Inorganic Fertilizer-Reduced Tillage) splits them then duplicates the
#' observation for the number of practice x product combinations. Values in the related practice and product  name and code columns are renamed
#'  to one of the single practice x single product combinations.
#'
#' @param Data the `ERA.Compiled` dataset.
#' @return `ERAComboSplit` returns a longer form version of `ERA.Compiled` with suffixed `.Combo` columns appended indicating a single practice x product
#' combination in duplicate rows of the split dataset.
#' @export
ERAComboSplit<-function(Data){

  X<-strsplit(unlist(Data[,SubPrName]),"-")
  Len<-rep(1:nrow(Data),unlist(lapply(X,length)))
<<<<<<< HEAD
  Data<-Data[Len]
  Data[,SubPrName.Combo:=unlist(X)]
=======
  Data<-Data[Len,SubPrName.Combo:=unlist(X)]
>>>>>>> 4836cfb62c94b284bd90ca907ef06c7666767542

  PracticeCodes<-data.table(PracticeCodes)
  Data[,SubPrName.Code.Combo:=PracticeCodes[match(Data[,SubPrName.Combo],Subpractice.S),Subpractice.Code]]

  Data[,PrName.Combo:=PracticeCodes[match(Data[,SubPrName.Combo],Subpractice.S),Practice]]
  Data[,PrName.Code.Combo:=PracticeCodes[match(Data[,SubPrName.Combo],Subpractice.S),Practice.Code]]

  Data[,Theme.Combo:=PracticeCodes[match(Data[,SubPrName.Combo],Subpractice.S),Theme]]
  Data[,Theme.Code.Combo:=PracticeCodes[match(Data[,SubPrName.Combo],Subpractice.S),Theme.Code]]


  X<-strsplit(Data[,Product.Simple],"-")
  Len<-rep(1:nrow(Data),unlist(lapply(X,length)))
<<<<<<< HEAD
  Data<-Data[Len]
  Data[,Product.Simple.Combo:=unlist(X)]
=======
  Data<-Data[Len,Product.Simple.Combo:=unlist(X)]
>>>>>>> 4836cfb62c94b284bd90ca907ef06c7666767542

  EUCodes<-data.table(EUCodes)
  Data[,Product.Code.Combo:=EUCodes[match(Data[,Product.Simple.Combo],Product.Simple),Product.Simple.Code]]

  Data[,Product.Subtype.Combo:=EUCodes[match(Data[,Product.Simple.Combo],Product.Simple),Product.Subtype]]
  Data[,Product.Subtype.Combo.Code:=EUCodes[match(Data[,Product.Simple.Combo],Product.Simple),Product.Subtype.Code]]

  Data[,Product.Type.Combo:=EUCodes[match(Data[,Product.Simple.Combo],Product.Simple),Product.Type]]
  Data[,Product.Type.Combo.Code:=EUCodes[match(Data[,Product.Simple.Combo],Product.Simple),Product.Type.Code]]

  return(Data)

}
