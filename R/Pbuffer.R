#' Create Circular Buffers from Point Data
#'
#' This function generates circular buffers around geographic points from a table containing latitude, longitude, and buffer radius information.
#' The input data must include `Latitude` and `Longitude` columns in decimal degrees and a numeric `Buffer` column indicating the radius in meters.
#' Optionally, an `ID` column can be provided to distinguish points with identical locations or buffer sizes.
#'
#' @param Data A `data.frame` or `data.table` containing point coordinates (`Latitude`, `Longitude`) in decimal degrees and `Buffer` in meters.
#'    NA values are removed.
#' @param ID Optional. The column name for grouping points with identical locations. Default is `NA`.
#' @param Projected Logical. If `TRUE`, the output will be in the projected coordinate system `EPSG:3395` (Mercator projection).
#'    If `FALSE`, the output will be in geographic coordinates `EPSG:4326` (WGS84).
#' @return A `SpatVect` object (from the `terra` package) representing the circular buffer polygons.
#' @export
#' @import terra
Pbuffer<-function(Data, ID = NA, Projected = FALSE) {
  Data <- data.frame(Data)
  # Filtering the Data
  Data <- Data[!(is.na(Data$Latitude) | is.na(Data$Longitude) |
                   Data$Buffer == "" | is.na(Data$Buffer)), ]
  # Handling ID selection
  if (!is.na(ID)) {
    SS <- unique(Data[, c("Latitude", "Longitude", ID, "Buffer")])
  } else {
    SS <- unique(Data[, c("Latitude", "Longitude", "Buffer")])
  }
  # Setting coordinate reference systems
  CRS.old <- "EPSG:4326" # WGS84
  CRS.new <- "EPSG:3395" # Mercator projection

  # Create points from latitude and longitude
  points <- terra::vect(cbind(SS$Longitude, SS$Latitude), crs = CRS.old, type="points")

  # Transforming points
  points <- terra::project(points, CRS.new)

  # Buffering and creating polygons
  pbuf1 <- terra::vect(lapply(1:nrow(SS), function(i) {
    pbuf <- terra::buffer(points[i, ], width = as.numeric(SS$Buffer[i]))
    pbuf
  }))

  pbuf1<-cbind(pbuf1,SS)

  # Projecting back if not Projected
  if (!Projected) {
    pbuf1 <- terra::project(pbuf1, CRS.old)
  }

  return(pbuf1)
}
