## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,include=F,eval=T,echo=T--------------------------------------------
require(ERAg)
require(broom)
require(ggplot2)
require(metafor)
require(metan)
require(pbapply)
require(plyr)
require(sfsmisc)
require(MASS)

## ----Access ERA, echo=T-------------------------------------------------------
knitr::kable(head(ERA.Compiled[,1:8], 5))

## ----Show Practice Codes, echo=T----------------------------------------------
knitr::kable(head(PracticeCodes[,1:6], 5))

## ----Show ERA Concepts, echo=T------------------------------------------------
ERAConcepts

