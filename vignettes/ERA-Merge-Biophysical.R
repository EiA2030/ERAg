## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load packages, eval=F,echo=T---------------------------------------------
#    if(!require("pacman", character.only = TRUE)){
#      install.packages("pacman",dependencies = T)
#    }
#  
#    required.packages <- c("ERAg","data.table","miceadds","dplyr")
#    p_load(char=required.packages,install = T,character.only = T)
#  

## ----load packages real, eval=T,echo=F,include=F------------------------------
  require(ERAg)
  require(data.table)

## ----create dirs, eval=F,echo=T-----------------------------------------------
#  # Create a directory for the climate analysis
#  ClimDir<-"ERA Climate Analysis/422814143322303020152010130-3POWCHIFALSE0.6/"
#  if(!dir.exists(ClimDir)){
#    dir.create(ClimDir)
#  }

## ----Load ClimateA, eval=F,echo=T---------------------------------------------
#  ClimateA<-load(paste0(ClimDir,"ClimStatsA.RData"))

## ----Load ERA, eval=F,echo=T--------------------------------------------------
#  ERA<-load(paste0(ClimDir,"Data.RData"))

