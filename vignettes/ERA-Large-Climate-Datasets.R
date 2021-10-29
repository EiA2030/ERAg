## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages,include=F,eval=T,echo=T-----------------------------------------
require(ERAg)
require(piggyback)

## ----Create Dir, echo =TRUE,eval=F--------------------------------------------
#  LargeDir<-"Large Files/"
#  if(!dir.exists(LargeDir)){
#    dir.create(LargeDir,recursive = T)
#  }
#  

## ----Set parameters, echo =TRUE,eval=F----------------------------------------
#  REPO<-"peetmate/ERAg"
#  TAG<-"Beta_v0.0.1"

## ----download large file, echo =TRUE,eval=F-----------------------------------
#  
#  Sys.setenv(GITHUB_TOKEN = "ghp_XXXXXXXXXXXXXXXXXXXXXXXX")
#  
#  piggyback::pb_download(file= "AgMERRA.RData",
#              repo = REPO,
#              tag = TAG,
#              dest = LargeDir)

