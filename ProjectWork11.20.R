##Plotting land cover of the colorado river basin for 2000
#Reading in colorado river basin shapefile
library(sf)
basin_shp <- st_read("Z:/omcmorrow/Project_Folder/CRB_Outline")
#Changing the coordinate projection system of the shapefile
st_crs(basin_shp)
reproject_basin <- st_transform(basin_shp, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#Reading in land cover data for 2000
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


#Repeating all steps for 2024 raster
rast_2024 <- rast("Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Annual_NLCD_LndCov_2024_CU_C1V1.tif")
crs(rast_2024)
st_crs(basin_shp)
basin_shp_proj <- st_transform(basin_shp, crs(rast_2024))
crop_rast_2024 <- crop(rast_2024, basin_shp_proj)
mask_rast_2024 <- mask(crop_rast_2024, basin_shp_proj)
plot(mask_rast_2024, main = "Colorado River Basin Land Cover (2024)")
writeRaster(mask_rast_2024, "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Masked_2024_rast.tif", overwrite = TRUE)
real_filepath_2024 <- "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Masked_2024_rast.tif"
copy_filepath_2024 <- "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Masked_2024_rast.copy.tif"
createCopy(format = "GTiff", dst_filename = copy_filepath_2024, src_filename = real_filepath_2024)
copy_mask_rast_2024 <- rast("Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Masked_2024_rast.copy.tif")
class_counts_2024 <- freq(copy_mask_rast_2024)
print(class_counts_2024)
rcl_matrix <- matrix(c(
  0, 79, 1,
  84, 95, 1,
  80, 83, 2
), ncol = 3, byrow = TRUE)
reclass_2024 <- classify(copy_mask_rast_2024, rcl_matrix, right = TRUE)
reclass_counts_2024 <- freq(reclass_2024)
print(reclass_counts_2024)
landcov_cols <- c("#C8BAAE", "yellow")
landcov_classes <- c("Non-Agricultural Land", "Agricultural Land\n(pasture/hay/cultivated crops)")
par(mar = c(5, 4, 4, 8), xpd = TRUE)
plot(reclass_2024, col = landcov_cols, main = "Colorado River Basin Agricultural vs Non-Agricultural Land Cover (2024)", legend = FALSE)
legend("topleft", inset = c(0.22, 0), legend = landcov_classes, fill= landcov_cols ,bty="n", cex = 0.7)
