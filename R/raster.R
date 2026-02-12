#' Create Area Raster
#'
#' Creates a raster where each cell contains its exact surface
#' area based on WGS84 ellipsoid calculations.
#' The raster covers a rectangular area defined by origin,
#' resolution, and size.
#'
#' @param origin Numeric vector of length 2. Origin point
#'   (xmin, ymax) in degrees
#' @param res Numeric vector of length 1 or 2. Resolution
#'   (x, y) in degrees
#' @param size Numeric vector of length 2. Number of columns
#'   and rows (ncols, nrows)
#' @param a_fact Numeric. Area scaling factor
#'   (default: 1e-4 for hectares)
#'
#' @return A terra raster object with "area" as the
#'   variable name
#'
#' @examples
#' \dontrun{
#' # Create a 10x10 cell area raster at 1-degree resolution
#' r <- make_area_rast(
#'   origin = c(-120, 50), res = 1,
#'   size = c(10, 10)
#' )
#' }
#'
#' @export
make_area_rast <- function(origin, res, size, a_fact = 1e-4) {
  # Latitudinal edges (y_edges): from top to bottom
  y_edges <- seq(from = origin[2], by = -res[2], length.out = size[2] + 1)

  # Calculation of areas per row
  row_vals <- .calc_y_area(y_edges, res[1], a_fact)

  # Vector expansion to fill columns
  vals <- rep(row_vals, each = size[1])

  ex_obj <- terra::ext(
    origin[1],
    origin[1] + (size[1] * res[1]),
    y_edges[size[2] + 1],
    origin[2]
  )

  r <- terra::rast(
    nrows = size[2],
    ncols = size[1],
    extent = ex_obj,
    crs = "EPSG:4326",
    vals = vals
  )

  names(r) <- "area"
  r
}

#' Convert Polygon to Area Raster
#'
#' Creates an area raster that covers the extent of a given
#' polygon. The polygon must be in WGS84 geographic
#' coordinates for accurate ellipsoid-based area
#' calculations.
#'
#' @param cell A terra spatial object (polygon) defining the
#'   area of interest
#' @param res Numeric vector of length 1 or 2. Resolution in
#'   degrees
#' @param a_fact Numeric. Area scaling factor
#'   (default: 1e-4 for hectares)
#'
#' @return A terra raster object with "area" as the
#'   variable name
#'
#' @details
#' The function validates that the input polygon is in
#' geographic coordinates (WGS84) and calculates the
#' optimal grid dimensions based on the specified
#' resolution. The resulting raster contains the exact
#' surface area for each cell.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' # Create a polygon and convert to area raster
#' poly <- ext(-120, -110, 40, 50) |>
#'   as.polygons() |>
#'   set.crs("EPSG:4326")
#' r <- poly_to_area_rast(poly, res = 0.1)
#' }
#'
#' @export
poly_to_area_rast <- function(cell, res, a_fact = 1e-4) {
  # TODO: must be WGS84!
  # CRS Validation
  if (!terra::is.lonlat(cell)) {
    stop("cell must be in geographic coordinates (WGS84)")
  }

  if (length(res) == 1) res <- c(res, res)

  e_obj <- terra::ext(cell)

  width <- e_obj$xmax - e_obj$xmin
  height <- e_obj$ymax - e_obj$ymin

  nx <- round(width / res[1])
  ny <- round(height / res[2])

  if (nx < 1 || ny < 1) {
    stop("res is larger than the polygon dimensions")
  }

  r <- make_area_rast(
    origin = c(e_obj$xmin, e_obj$ymax),
    res = res,
    size = c(nx, ny),
    a_fact = a_fact
  )

  r
}
