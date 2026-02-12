# Example Pipeline for zn84 Package

library(terra)
library(zn84)

# Read a GeoPackage file with polygons (e.g., administrative boundaries)
# polygons <- terra::vect("path/to/your/polygons.gpkg")

# Read a TIFF file with categorical data (e.g., land cover classification)
# classification_rast <- terra::rast("path/to/your/classification.tif")

# For demonstration, create sample data
set.seed(123)
polygons <- terra::vect(
  "POLYGON ((-120 40, -119 40, -119 41, -120 41, -120 40)),
   POLYGON ((-118.5 40, -117.5 40, -117.5 41, -118.5 41, -118.5 40)),
   POLYGON ((-117 40, -116 40, -116 41, -117 41, -117 40))",
  crs = "EPSG:4326"
)

# Create a sample classification raster
r_map <- terra::rast(
  matrix(
    data = sample(1:4, 100, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2)),
    nrow = 10,
    ncol = 10
  ),
  extent = terra::ext(-120.5, -115.5, 39.5, 41.5),
  crs = "EPSG:4326"
)

# Assign class names (e.g., land cover types)
class_names <- c("Forest", "Agriculture", "Urban", "Water")
names(class_names) <- 1:4

# 2. Process each polygon
results <- list()

for (i in 1:length(polygons)) {
  cat("Processing polygon", i, "of", length(polygons), "\n")

  # Extract individual polygon
  cell <- polygons[i]

  # Convert polygon to area raster
  r_area <- poly_to_area_rast(cell, res = 0.1)

  # Calculate zonal statistics
  stats <- zn_stats(r_area, r_map)

  # Add polygon ID and class names
  stats$id <- i

  # Save results (in practice, you might save to disk)
  results[[i]] <- stats
}

results <- do.call(rbind, results)
print(results)

plot(r_map)
plot(r_area)

# 4. Optional: Save results to disk
# terra::writeVector(results, "results.gpkg", overwrite = TRUE)
