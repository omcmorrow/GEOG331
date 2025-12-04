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
#Copying the masked raster for reclassification
copy_filepath <- "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data"
writeRaster(mask_rast_2000, filename = copy_filepath, format = "GTiff", overwrite = TRUE)
#Reclassifying raster classes to agriculture and non agriculture
class_counts <- freq(mask_rast_2000)
print(class_counts)
rcl_matrix <- matrix(c(
  0, 12, 1,
  15, 16, 1,
  13, 14, 2
), ncol = 3, byrow = TRUE)