#' The compiled ERA dataset
#'
#' \describe{
#'   \item{Index}{unique row identity}
#'   \item{Code}{unique publication identity}
#'   \item{Author}{primary author of the publication}
#'   ...
#' }
#' @source *To be added*
"ERA.Compiled"
#' Practice Codes
#'
#' \describe{ The organizational hierarchy of improved farming practice concepts considered in ERA and their descriptions.
#'   \item{Code}{unique alpha numeric code}
#'   \item{Theme}{concepts in the highest level in the practice hierarchy}
#'   \item{Theme.Code}{currently a duplicate of theme}
#'   \item{Practice}{concepts in the intermediate level of the practice hierarchy. Practice is nested under Theme.}
#'   \item{Practice.Code}{short code corresponding to the practice name for use in plotting}
#'   \item{Subpractice}{concepts in the lowest and most detailed level of the practice hierarchy. Subpractice is nested under practice.}
#'   \item{Subpractice.Code}{short code corresponding to the subpractice name for use in plotting}
#'   \item{Subpractice.S}{shorter name for subpractice}
#'   ...
#' }
#' @source *To be added*
"PracticeCodes"
#' Bioclimatic Variables
#'
#' \describe{ There are  19 “bioclimatic” variables which are derived from the monthly temperature and rainfall values in order to generate biologically meaningful variables.
#' These are often used in species distribution modeling and related ecological modeling techniques. The bioclimatic variables represent annual
#' trends (e.g., mean annual temperature, annual precipitation) seasonality (e.g., annual range in temperature and precipitation) and extreme or
#' limiting environmental factors (e.g., temperature of the coldest and warmest month, and precipitation of the wet and dry quarters). A quarter
#' is a period of three months (1/4 of the year).
#' Bioclim data are extracted and summmarized for each unique ERA location plus its buffer of spatial uncertainty.
#'   \item{bio_1}{Annual Mean Temperature}
#'   \item{bio_2}{Mean Diurnal Range (Mean of monthly (max temp - min temp))}
#'   \item{bio_3}{Isothermality (BIO2/BIO7) (* 100)}
#'   \item{bio_4}{Temperature Seasonality (standard deviation *100)}
#'   \item{bio_5}{Max Temperature of Warmest Month}
#'   \item{bio_6}{Min Temperature of Coldest Month}
#'   \item{bio_7}{Temperature Annual Range (BIO5-BIO6)}
#'   \item{bio_8}{Mean Temperature of Wettest Quarter}
#'   \item{bio_9}{Mean Temperature of Driest Quarter}
#'   \item{bio_10}{Mean Temperature of Warmest Quarter}
#'   \item{bio_11}{Mean Temperature of Coldest Quarter}
#'   \item{bio_12}{Annual Precipitation}
#'   \item{bio_13}{Precipitation of Wettest Month}
#'   \item{bio_14}{Precipitation of Driest Month}
#'   \item{bio_15}{Precipitation Seasonality (Coefficient of Variation)}
#'   \item{bio_16}{Precipitation of Wettest Quarter}
#'   \item{bio_17}{Precipitation of Driest Quarter}
#'   \item{bio_18}{Precipitation of Warmest Quarter}
#'   \item{bio_19}{Precipitation of Coldest Quarter}
#'   ...
#' }
#' @source https://www.worldclim.org/data/bioclim.html version 2.1 climate data for 1970-2000, released in January 2020.
"ERA_BioClim"
#' Landcover
#'
#' \describe{
#'  The CCI-LC project delivers a new time series of 24 consistent global LC maps at 300 m spatial resolution on an annual basis from 1992 to 2015.
#'  The number of raster cells of each landcover class for the year 2015 is summed for each unique ERA location and its buffer of spatial uncertainty.
#'  A description of the landcover classes (fields) in this dataset can be found in the *`ERA_CCI_LC_15_Fields`* object.
#'   ...
#' }
#' @source CCI-LC Land Cover Maps - v2.0.7 http://maps.elie.ucl.ac.be/CCI/viewer/download.php
"ERA_CCI_LC_15"
#' ISDA Soil Parameters
#'
#' \describe{
#' Using the  \href{African Soil and Agronomy Data Cube}{https://gitlab.com/openlandmap/africa-soil-and-agronomy-data-cube}
#' \href{ISDA}{https://www.isda-africa.com/isdasoil/} soil data were downloaded and summarized for each unique ERA locations and its buffer of spatial
#' uncertainty.
#'
#' Variable information & descriptions can be found on the African Soiland Agronomy Data Cube.
#'   ...
#' }
#' @source https://www.isda-africa.com/isdasoil/
"ERA_ISDA"

