#' Calculate Area for Latitude Bands
#'
#' Internal function to calculate the true surface area of
#' latitude bands on the WGS84 ellipsoid.
#' Uses the ellipsoid integral formula for accurate area
#' computation.
#'
#' @param y_edges Numeric vector of latitude edges in degrees
#' @param x_res Numeric. Longitude resolution in degrees
#' @param factor Numeric. Scaling factor to convert square meters
#'   to desired units
#'
#' @return Numeric vector of areas for each latitude band
#'
#' @keywords internal
.calc_y_area <- function(y_edges, x_res, factor) {
  # WGS84 ellipsoid parameters
  a <- 6378137.0
  f <- 1 / 298.257223563
  b <- a * (1 - f)
  e <- sqrt(f * (2 - f))

  phi <- y_edges * (pi / 180)
  d_lam <- x_res * (pi / 180)

  s_phi <- sin(phi)
  e_s <- e * s_phi

  # Ellipsoid integral area
  q_val <- (s_phi / (1 - e_s^2)) + (1 / (2 * e)) * log((1 + e_s) / (1 - e_s))

  # Difference between adjacent boundaries (North - South)
  areas <- abs(diff(q_val)) * ((b^2 * d_lam) / 2)

  areas * factor
}
