## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages,include=F,eval=T,echo=T-----------------------------------------
require(ERAg)
require(tidyr)
require(treemap)
require(spatstat)

## ----Combo numbers, echo =TRUE------------------------------------------------
# No. combination practices
nrow(ERA.Compiled[grepl("-",SubPrName)])

# No. solo practices
nrow(ERA.Compiled[!grepl("-",SubPrName)])

