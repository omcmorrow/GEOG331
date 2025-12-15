###Manipulation of discharge data
#Reading in USGS discharge data
install.packages("dataRetrieval")
library(dataRetrieval)
library(dplyr)
library(lubridate)
datD <- read.csv("Z:/omcmorrow/Project_Folder/DailyDischargeDataLagunaDam/DailyDischargeLagunaDam2000_2024.csv",stringsAsFactors = T)
#Reading column headers for data
head(datD)
#Formatting date in order to group by year and month
datD$Date <- as.Date(datD$time)
head(datD)
#Removing the first day of 2025 since this is extraneous data
day_to_remove <- as.Date("2025-01-01")
datDnew <- datD %>%
  filter(Date != day_to_remove)
#Converting cfs discharge data to cms (cubic meters / second)
multiplier1 <- 0.0283168 #scalefactor between cfs and cms
datDnew$value <- datDnew$value * multiplier1
datDis <- datDnew %>%
  rename(discharge_cms = value)
#Extracting monthly max and min discharge (2000-2024)
monthly_summary <- datDis %>%
  mutate(year_month = floor_date(Date, "month")) %>%
  group_by(year_month) %>%
  summarise(
    monthly_max = max(discharge_cms, na.rm = TRUE),
    monthly_min = min(discharge_cms, na.rm = TRUE),
    monthly_sum = sum(discharge_cms, na.rm = TRUE)
  )
print(monthly_summary, n = 300)
#Plotting monthly max and min discharge (2000-2024)
library(ggplot2)
ggplot(data = monthly_summary, aes(x = year_month)) +
  geom_line(aes(y = monthly_max, color = "Monthly Max"), linewidth = 1) +
  geom_line(aes(y = monthly_min, color = "Monthly Min"), linewidth = 1) +
  scale_color_manual(name = "Discharge", values = c("Monthly Max" = "blue", "Monthly Min" = "red")) +
  labs(title = "Monthly Max and Min Discharge for the Colorado River Below Laguna Dam, AZ-CA (2000-2024)",
       x = "Date", y = expression(paste("Discharge m"^"3 ","sec"^"-1"))) + theme_minimal()


###Manipulation of discharge and precipitation data to normalize discharge based on precipitation
#Reading in precipitation data for the upper and lower basins (2000-2024)
datUp <- read.csv("Z:/omcmorrow/Project_Folder/PrecipitationData/Upper_CRB_Precip_2000-2024.csv",stringsAsFactors = T)
datLow <- read.csv("Z:/omcmorrow/Project_Folder/PrecipitationData/Lower_CRB_Precip_2000-2024.csv",stringsAsFactors = T)
#Changing upper basin to numeric format
datUp$X...Upper.Colorado.River.Basin.Precipitation <- as.character(datUp$X...Upper.Colorado.River.Basin.Precipitation)
datUp$X...Upper.Colorado.River.Basin.Precipitation <- as.numeric(datUp$X...Upper.Colorado.River.Basin.Precipitation)
#Changing lower basin to numeric format
datLow$X...Lower.Colorado.River.Basin.Precipitation <- as.character(datLow$X...Lower.Colorado.River.Basin.Precipitation)
datLow$X...Lower.Colorado.River.Basin.Precipitation <- as.numeric(datLow$X...Lower.Colorado.River.Basin.Precipitation)
#Joining together both upper and lower basin precip values
colnames(datUp) <- c("Precip")
print(datUp)
colnames(datLow) <- c("Precip")
print(datLow)
dat_total_precip <- datUp
dat_total_precip$SummedPrecip <- datUp$Precip + datLow$Precip
dat_total_precip$Precip <- NULL
print(dat_total_precip)
#Converting precip from inches to meters
multiplier2 <- 0.0254
dat_total_precip$SummedPrecip <- dat_total_precip$SummedPrecip * multiplier2
dat_precip <- dat_total_precip %>%
  rename(precip_m = SummedPrecip)
#Calculating volume of rainfall from precip and basin area
multiplier3 <- 640000000000
dat_precip$volume_m3 <- dat_precip$precip_m * multiplier3
print(dat_precip)
#Creating precip volume column to be joined to discharge
monthly_precip_vol <- dat_precip[-c(1,2),2]
#Joining total monthly precip volume to monthly discharge sums
monthly_summary$monthly_precip_vol <- monthly_precip_vol
print(monthly_summary, n = 300)
#Calculating runoff ratio (volume discharge / volume precipitation) to normalize discharge
monthly_summary$runoffratio <- monthly_summary$monthly_sum / monthly_summary$monthly_precip_vol * 100
print(monthly_summary, n = 300)
#Plotting runoff ratio
ggplot(data = monthly_summary, aes(year_month, runoffratio)) +
  geom_line() + theme_minimal() + labs(title = "Colorado River Basin Monthly Runoff Ratio Below Laguna Dam, AZ-CA (2000-2024)", x = "Date", y = "Runoff Ratio")
#Saving monthly summary discharge and precip dataframe
install.packages("readr")
library(readr)
write_csv(monthly_summary, "Z:/omcmorrow/Project_Folder/JoinedDisPrecipData/MonthlySummary.csv")


#Finding annual sums of discharge
annual_summary <- datDis %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(annual_sum = sum(discharge_cms, na.rm = TRUE))
#Matching dataframe to have same years as land cover data
years_to_remove <- c(2001,2002,2003,2004,2005,2007,2008,2009,2010,2011,2013,2014,2015,2016,2017,2019,2020,2021,2022,2023)
annual_summary <- annual_summary %>% filter(!year %in% years_to_remove)
annual_summary$precip_m <- c(0.630936, 0.66167, 0.51562, 0.600964, 0.646684)
annual_summary$volume_m3 <- annual_summary$precip_m * multiplier3
annual_summary$runoffratio <- annual_summary$annual_sum / annual_summary$volume_m3
#Saving dataframe
write_csv(annual_summary, "Z:/omcmorrow/Project_Folder/JoinedDisPrecipData/AnnualSummary.csv")


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
start_year <- 2003
end_year <- 2024
years <- start_year:end_year
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


#Reading in masked land cover data and monthly discharge + precip data
library(dataRetrieval)
library(dplyr)
library(lubridate)
library(terra)
library(tidyterra)
library(FedData)
dat_annual_summary <- read.csv("Z:/omcmorrow/Project_Folder/JoinedDisPrecipData/AnnualSummary.csv")
#Creating dataframe for agriculture pixel count by year
year <- c("2000", "2006", "2012", "2018", "2024")
agriculture <- c("15686991", "15064903", "14844945", "14744004", "14015389")
dat_ag <- data.frame(Year = year, AgriculturePixels = agriculture)
multiplier1 <- 900
numeric_vec <- as.numeric(dat_ag$AgriculturePixels)
dat_ag$Agriculture_sq_m <- numeric_vec * multiplier1
print(dat_ag)
#Fitting a regression model
fit <- lm(dat_annual_summary$runoffratio ~ dat_ag$Agriculture_sq_m)
#Plotting residuals
plot(dat_annual_summary$runoffratio, summary(fit)$residuals, pch = 19,
     xlab="Runoff Ratio", ylab="Residuals")
abline(h = 0)
#Checking normality of residuals
hist(summary(fit)$residuals, col ="red",
     main="Residual Distribution", xlab="Residuals")
#Checking with qqnorm and qqline
qqnorm(summary(fit)$residuals, pch = 19)
qqline(summary(fit)$residuals, pch = 19)
#Using Shapiro Wilks test
shapiro.test(summary(fit)$residuals)
model_summary <- summary(fit)
print(model_summary)
