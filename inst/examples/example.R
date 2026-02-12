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
classification_rast <- terra::rast(
  matrix(sample(1:4, 100, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2)), 10, 10),
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
  polygon <- polygons[i]
  
  # 2.1. Convert polygon to area raster
  area_rast <- poly_to_area_rast(polygon, res = 0.1)
  
  # 2.2. Calculate zonal statistics
  stats <- zn_stats(area_rast, classification_rast)
  
  # Add polygon ID and class names
  stats$polygon_id <- i
  
  # 2.3. Save results (in practice, you might save to disk)
  # For this example, store in results list
  results[[i]] <- stats
  
  # Print summary
  cat("  Total area:", round(stats$abs_total, 2), "hectares\n")
  
  # Print class areas if available
  class_cols <- grep("^abs_", names(stats), value = TRUE)
  if (length(class_cols) > 0) {
    for (col in class_cols) {
      class_id <- sub("^abs_", "", col)
      class_name <- class_names[as.numeric(class_id)]
      area_val <- stats[[col]]
      if (!is.na(area_val) && area_val > 0) {
        cat("  ", class_name, ":", round(area_val, 2), "hectares\n")
      }
    }
  }
  cat("\n")
}

# 3. Combine all results
if (length(results) > 1) {
  combined_results <- do.call(rbind, lapply(results, as.data.frame))
  cat("Combined results for all", length(polygons), "polygons:\n")
  print(combined_results)
} else {
  cat("Single polygon result:\n")
  print(as.data.frame(results[[1]]))
}

# 4. Optional: Save results to disk
# saveRDS(combined_results, "zonal_statistics_results.rds")
# terra::writeVector(combined_results, "zonal_statistics_results.gpkg", overwrite = TRUE)
