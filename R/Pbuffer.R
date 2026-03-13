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
Pbuffer <- function(Data, ID = NA, Projected = FALSE) {
  Data <- data.frame(Data)

  Data$Latitude  <- suppressWarnings(as.numeric(Data$Latitude))
  Data$Longitude <- suppressWarnings(as.numeric(Data$Longitude))
  Data$Buffer    <- suppressWarnings(as.numeric(Data$Buffer))

  # Keep only valid geographic coordinates and valid buffers
  keep <- !is.na(Data$Latitude) &
    !is.na(Data$Longitude) &
    !is.na(Data$Buffer) &
    is.finite(Data$Latitude) &
    is.finite(Data$Longitude) &
    is.finite(Data$Buffer) &
    Data$Buffer > 0 &
    abs(Data$Longitude) <= 180 &
    abs(Data$Latitude) <= 90

  Data <- Data[keep, , drop = FALSE]

  if (!is.na(ID)) {
    SS <- unique(Data[, c("Latitude", "Longitude", ID, "Buffer"), drop = FALSE])
  } else {
    SS <- unique(Data[, c("Latitude", "Longitude", "Buffer"), drop = FALSE])
  }

  if (nrow(SS) == 0) {
    stop("Pbuffer: no valid lon/lat rows remain after filtering.")
  }

  CRS.old <- "EPSG:4326"
  CRS.new <- "EPSG:3395"

  points <- terra::vect(
    SS[, c("Longitude", "Latitude"), drop = FALSE],
    geom = c("Longitude", "Latitude"),
    crs = CRS.old
  )
  points <- terra::project(points, CRS.new)

  buf_list <- vector("list", nrow(SS))

  for (i in seq_len(nrow(SS))) {
    buf_i <- terra::buffer(points[i], width = SS$Buffer[i])

    if (nrow(buf_i) < 1) {
      next
    }

    att_i <- SS[rep(i, nrow(buf_i)), , drop = FALSE]
    rownames(att_i) <- NULL

    buf_i <- cbind(buf_i, att_i)
    buf_list[[i]] <- buf_i
  }

  buf_list <- Filter(Negate(is.null), buf_list)

  if (length(buf_list) == 0) {
    stop("Pbuffer: no buffers could be created.")
  }

  pbuf1 <- do.call(rbind, buf_list)

  if (!Projected) {
    pbuf1 <- terra::project(pbuf1, CRS.old)
  }

  pbuf1
}
