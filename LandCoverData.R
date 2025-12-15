##Plotting land cover of the colorado river basin for 2000
#Reading in colorado river basin shapefile
library(sf)
basin_shp <- st_read("Z:/omcmorrow/Project_Folder/CRB_Shapefile")
#Changing the coordinate projection system of the shapefile
st_crs(basin_shp)
reproject_basin <- st_transform(basin_shp, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#Reading in land cover data for 2000
install.packages("raster")
library(terra)
library(tidyterra)
library(FedData)
rast_2000 <- rast("Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Annual_NLCD_LndCov_2000_CU_C1V1.tif")
#Changing coordinate systems to match
crs(rast_2000)
st_crs(basin_shp)
basin_shp_proj <- st_transform(basin_shp, crs(rast_2000))
#Cropping and masking raster to basin shapefile
crop_rast_2000 <- crop(rast_2000, basin_shp_proj)
mask_rast_2000 <- mask(crop_rast_2000, basin_shp_proj)
#Plotting land cover
plot(mask_rast_2000, main = "Colorado River Basin Land Cover (2000)")
#Saving new masked raster
writeRaster(mask_rast_2000, "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Masked_2000_rast.tif", overwrite = TRUE)
#Copying the masked raster for reclassification
install.packages("gdalraster")
library(gdalraster)
real_filepath_2000 <- "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Masked_2000_rast.tif"
copy_filepath_2000 <- "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Masked_2000_rast.copy.tif"
createCopy(format = "GTiff", dst_filename = copy_filepath_2000, src_filename = real_filepath_2000)
#Reclassifying raster classes to agriculture and non agriculture
copy_mask_rast_2000 <- rast("Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Masked_2000_rast.copy.tif")
class_counts_2000 <- freq(copy_mask_rast_2000)
print(class_counts_2000)
rcl_matrix <- matrix(c(
  0, 79, 1,
  84, 95, 1,
  80, 83, 2 #classes 81 and 82 are pasture/hay and cultivated crops to be aggregated as agriculture land cover
), ncol = 3, byrow = TRUE)
reclass_2000 <- classify(copy_mask_rast_2000, rcl_matrix, right = TRUE)
reclass_counts_2000 <- freq(reclass_2000)
print(reclass_counts_2000)
landcov_cols <- c("#C8BAAE", "yellow")
landcov_classes <- c("Non-Agricultural Land", "Agricultural Land\n(pasture/hay/cultivated crops)")
par(mar = c(5, 4, 4, 8), xpd = TRUE)
plot(reclass_2000, col = landcov_cols, main = "Colorado River Basin Agricultural vs Non-Agricultural Land Cover (2000)", legend = FALSE)
legend("topleft", inset = c(0.22, 0), legend = landcov_classes, fill= landcov_cols ,bty="n", cex = 0.7)
#Saving final raster
writeRaster(reclass_2000, "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Reclassed_2000_rast.tif", overwrite = TRUE)


#Repeating steps for all other rasters
years <- c(2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
library(raster)
base_url <- "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Annual_NLCD_LndCov_"
file_extension <- "_CU_C1V1.tif"
base_output <- "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Reclassed_"
#For loop
for (year in years) {
input_file <- paste0(base_url, year, file_extension)
current_raster <- raster(input_file)
crs(current_raster)
st_crs(basin_shp)
basin_shp_proj <- st_transform(basin_shp, crs(current_raster))
crop_current_rast <- crop(current_raster, basin_shp_proj)
mask_current_rast <- mask(crop_current_rast, basin_shp_proj)
rcl_matrix <- matrix(c(
  0, 79, 1,
  84, 95, 1,
  80, 83, 2
), ncol = 3, byrow = TRUE)
reclass_current_rast <- reclassify(mask_current_rast, rcl_matrix, right = TRUE)
output_file <- paste0(base_output, year, "_rast.tif")
writeRaster(reclass_current_rast, filename = output_file, format = "GTiff", overwrite = TRUE)
message(paste0("Processed and saved raster for year: ", year))
}

rast_2024 <- rast("Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Reclassed_2024_rast.tif")
landcov_cols <- c("#C8BAAE", "yellow")
landcov_classes <- c("Non-Agricultural Land", "Agricultural Land\n(pasture/hay/cultivated crops)")
par(mar = c(5, 4, 4, 8), xpd = TRUE)
plot(rast_2024, col = landcov_cols, main = "Colorado River Basin Agricultural vs Non-Agricultural Land Cover (2024)", legend = FALSE)
legend("topleft", inset = c(0.22, 0), legend = landcov_classes, fill= landcov_cols ,bty="n", cex = 0.7)
