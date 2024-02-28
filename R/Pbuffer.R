#' Buffer Points
#'
#' Creates circular buffers from a table containing point locations and buffer radii. The input table requires `Latitude` and `Longitude` columns in
#' decimal degrees and a numeric column named `Buffer` which indicates the buffer radius in meters.
#'
#' An optional `ID` field can be specified if there is need to separate any points with identical locations and buffers.
#'
#' @param Data A data.table or data.frame with decimal degree point locations in columns `Latitude` and `Longitude`, and a numeric column `Buffer` which indicates the buffer radius
#' to buffer each point in meters. NA values are not permitted and are filtered from the dataset.
#' @param ID The column name of any grouping variables used to split point x buffer locations. Default = NA.
#' @param Projected `Logical TRUE/FALSE`. If `TRUE` the output object has a projected CRS `+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs`,
#' if `FALSE` it has a geographic CRS `epsg:4326`.
#' @return Pbuffer returns circular buffers in an object of class `SpatialPolygons`.
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
  CRS.old <- "EPSG:4326" # Note: terra uses EPSG codes directly
  CRS.new <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

  # Creating points
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
