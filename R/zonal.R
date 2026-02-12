#' Calculate Zonal Statistics
#'
#' Performs zonal statistics using an area raster and a
#' classification map. Calculates both absolute and
#' proportional areas for each class in the classification
#' map.
#'
#' @param r_area A terra raster containing surface area
#'   values for each cell
#' @param r_map A terra raster with categorical values for
#'   zonal statistics
#' @param ... Additional arguments passed to terra::zonal()
#'
#' @return A terra polygon with zonal statistics including:
#'   - abs_total: Total valid area
#'   - abs_X: Absolute area for class X
#'   - prp_X: Proportional area for class X
#'
#' @details
#' The function projects the classification map to match the
#' area raster's CRS, masks the area values to the
#' classification extent, and calculates statistics for
#' each class. Results are returned as a polygon with the
#' same extent as the area raster.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' # Create area raster and classification map
#' area_rast <- make_area_rast(
#'   origin = c(-120, 50), res = 1,
#'   size = c(10, 10)
#' )
#' class_rast <- rast(
#'   matrix(
#'     sample(1:3, 100, replace = TRUE),
#'     10, 10
#'   ),
#'   ext = ext(area_rast), crs = "EPSG:4326"
#' )
#' # Calculate zonal statistics
#' stats <- zn_stats(area_rast, class_rast)
#' }
#'
#' @export
zn_stats <- function(r_area, r_map, ...) {
  # Parameter validation
  if (!inherits(r_area, "SpatRaster")) {
    stop("r_area must be a terra SpatRaster object")
  }
  if (!inherits(r_map, "SpatRaster")) {
    stop("r_map must be a terra SpatRaster object")
  }
  if (terra::nrow(r_area) == 0 || terra::ncol(r_area) == 0) {
    stop("r_area cannot be empty")
  }
  if (terra::nrow(r_map) == 0 || terra::ncol(r_map) == 0) {
    stop("r_map cannot be empty")
  }
  # Project r_map to r_area CRS
  r_map <- terra::project(r_map, r_area, method = "near")

  # Set NA values to r_area
  r_area_msk <- terra::mask(r_area, r_map)

  # Compute statistics per r_map class and sum r_area_msk
  stats <- terra::zonal(r_area_msk, r_map, fun = "sum", na.rm = TRUE, ...)

  if (nrow(stats) == 0) {
    geom <- terra::as.polygons(terra::ext(r_area), crs = terra::crs(r_area))
    geom$abs_total <- 0
    return(geom)
  }

  # Prepare result
  total_valid <- sum(stats[, 2])

  classes <- stats[, 1]
  abs_vals <- stats[, 2]
  prp_vals <- abs_vals / total_valid

  names(abs_vals) <- paste0("abs_", classes)
  names(prp_vals) <- paste0("prp_", classes)

  df_row <- data.frame(
    abs_total = total_valid,
    t(abs_vals),
    t(prp_vals)
  )

  v <- terra::as.polygons(terra::ext(r_area), crs = terra::crs(r_area))

  terra::values(v) <- df_row

  v
}
