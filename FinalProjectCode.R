#Reading in all required packages for full code
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(sf)
library(terra)
library(tidyterra)
library(raster)


###PART 1: Normalizing and visualizing discharge and precipitation data
##Visualizing discharge trends
#Reading in USGS discharge data
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
multiplier1 <- 0.0283168 #Scalefactor between cfs and cms
datDnew$value <- datDnew$value * multiplier1
datDis <- datDnew %>%
  rename(discharge_cms = value)
#Extracting monthly max and min and summed discharge
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
ggplot(data = monthly_summary, aes(x = year_month)) +
  geom_line(aes(y = monthly_max, color = "Monthly Max"), linewidth = 1) +
  geom_line(aes(y = monthly_min, color = "Monthly Min"), linewidth = 1) +
  scale_color_manual(name = "Discharge", values = c("Monthly Max" = "blue", "Monthly Min" = "red")) +
  labs(title = "Monthly Max and Min Discharge for the Colorado River Below Laguna Dam, AZ-CA (2000-2024)",
       x = "Date", y = expression(paste("Discharge m"^"3 ","sec"^"-1"))) + theme_minimal()

##Normalizing discharge with precipitation for monthly data
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
multiplier2 <- 0.0254 #Scalefactor between in and m
dat_total_precip$SummedPrecip <- dat_total_precip$SummedPrecip * multiplier2
#Creating new dataframe to reflect unit conversion
dat_precip <- dat_total_precip %>%
  rename(precip_m = SummedPrecip)
#Calculating volume of rainfall from precip and basin area
multiplier3 <- 640000000000 #Area of full basin in meters
dat_precip$volume_m3 <- dat_precip$precip_m * multiplier3
print(dat_precip)
#Removing first two NA rows from dat_precip due to original csv formatting
dat_monthly_precip <- dat_precip[-c(1,2), ]
print(dat_monthly_precip)
#Joining total monthly precip volume to monthly discharge summary dataframe
monthly_summary$monthly_precip_vol <- dat_monthly_precip$volume_m3
print(monthly_summary, n = 300)
#Calculating monthly runoff ratio (volume discharge / volume precipitation) to normalize discharge
monthly_summary$monthly_runoff_ratio <- monthly_summary$monthly_sum / monthly_summary$monthly_precip_vol * 100
print(monthly_summary, n = 300)
#Saving monthly summary discharge and precip dataframe
write_csv(monthly_summary, "Z:/omcmorrow/Project_Folder/JoinedDisPrecipData/MonthlySummary.csv")

##Normalizing discharge with precipitation for annual data
#Finding annual sums of discharge
annual_summary <- datDis %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(annual_sum_dis = sum(discharge_cms, na.rm = TRUE))
#Finding annual sums of precipitation
groups <- rep(1:ceiling(NROW(dat_monthly_precip$precip_m) / 12), each = 12, length.out = NROW(dat_monthly_precip$precip_m))
annual_sum_precip_m <- rowsum(dat_monthly_precip$precip_m, groups)
#Converting precipitation sum to a dataframe
annual_sum_precip_m <- as.data.frame(annual_sum_precip_m)
#Joining dataframes
annual_summary$annual_sum_precip_m <- annual_sum_precip_m$V1
#Finding annual sums of precipitation volume
annual_summary$annual_sum_volume_m3 <- annual_summary$annual_sum_precip_m * multiplier3
#Finding annual runoff ratio (volume discharge / volume precipitation) to normalize discharge
annual_summary$annual_runoff_ratio <- annual_summary$annual_sum_dis / annual_summary$annual_sum_volume_m3
#Saving dataframe
write_csv(annual_summary, "Z:/omcmorrow/Project_Folder/JoinedDisPrecipData/AnnualSummary.csv")

##Plotting monthly and annual runoff ratio
#Monthly runoff ratio
ggplot(data = monthly_summary, aes(year_month, monthly_runoff_ratio)) +
  geom_line() + theme_minimal() + labs(title = "Colorado River Basin Monthly Runoff Ratio Below Laguna Dam, AZ-CA (2000-2024)", x = "Date", y = "Runoff Ratio")
#Annual runoff ratio
ggplot(data = annual_summary, aes(year, annual_runoff_ratio)) +
  geom_line() + theme_minimal() + labs(title = "Colorado River Basin Annual Runoff Ratio Below Laguna Dam, AZ-CA (2000-2024)", x = "Date", y = "Runoff Ratio")


###Part 2: Reclassifying land cover rasters and visualizing land cover
#Reading in colorado river basin shapefile
basin_shp <- st_read("Z:/omcmorrow/Project_Folder/CRB_Shapefile")
#Changing the coordinate projection system of the shapefile
st_crs(basin_shp)
reproject_basin <- st_transform(basin_shp, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#Setting up parameters for a for loop
years <- c(2000,2003,2006,2009,2012,2015,2018,2021,2024)
base_url1 <- "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Annual_NLCD_LndCov_"
file_extension1 <- "_CU_C1V1.tif"
base_output <- "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Reclassed_"
#For loop to read in rasters and reclassify rasters
for (year in years) {
  input_file1 <- paste0(base_url, year, file_extension)
  current_raster1 <- raster(input_file1)
  crs(current_raster1)
  st_crs(basin_shp)
  basin_shp_proj <- st_transform(basin_shp, crs(current_raster1))
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

##Finding total pixel counts for all rasters
base_url2 <- "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Reclassed_"
file_extension2 <- "_rast.tif"
rast_counts_list <- list()
#For loop to read in rasters and find reclassified pixel counts
for (year in years) {
  input_file2 <- paste0(base_url2, year, file_extension2)
  current_raster2 <- rast(input_file2)
  class_counts <- freq(current_raster2)
  rast_counts_list[[year]] <- class_counts
}
#Combining freq tables into dataframe
dat_rast_counts <- do.call(rbind, rast_counts_list)
print(dat_rast_counts)
#Organizing dataframe
non_ag_land <- dat_rast_counts$count[seq(1, nrow(dat_rast_counts), 2)]
ag_land <- dat_rast_counts$count[seq(2, nrow(dat_rast_counts), 2)]
dat_rast_counts <- data.frame(year = years, ag_land = ag_land, non_ag_land = non_ag_land)
print(dat_rast_counts)
#Multiplying pixel counts by resolution to find total areas
multiplier4 <- 900 #Resolution of 30 m by 30 m 
dat_rast_counts$ag_land_area_m2 <- dat_rast_counts$ag_land * multiplier4
dat_rast_counts$non_ag_land_area_m2 <- dat_rast_counts$non_ag_land * multiplier4
print(dat_rast_counts)
#Saving dataframe
write.csv(dat_rast_counts, "Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Land_Cover_Summary.csv")

##Base code to plot any given year raster (change year when needed)
rast_2024 <- rast("Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Reclassed_2024_rast.tif")
landcov_cols <- c("#C8BAAE", "yellow")
landcov_classes <- c("Non-Agricultural Land", "Agricultural Land\n(pasture/hay/cultivated crops)")
par(mar = c(5, 4, 4, 8), xpd = TRUE)
plot(rast_2024, col = landcov_cols, main = "Colorado River Basin Agricultural vs Non-Agricultural Land Cover (2024)", legend = FALSE)
legend("topleft", inset = c(0.22, 0), legend = landcov_classes, fill= landcov_cols ,bty="n", cex = 0.7)


###Part 3: Performing linear regression analysis
#Reading in annual discharge + precip data
dat_annual_summary <- read.csv("Z:/omcmorrow/Project_Folder/JoinedDisPrecipData/AnnualSummary.csv")
#Matching annual discharge dataframe with the same years as land cover data
years_to_remove <- c(2001,2002,2004,2005,2007,2008,2010,2011,2013,2014,2016,2017,2019,2020,2022,2023)
dat_annual_summary <- annual_summary %>% filter(!year %in% years_to_remove)
#Reading in land cover summary data
dat_land_summary <- read.csv("Z:/omcmorrow/Project_Folder/NLCD_Land_Cover_Data/Land_Cover_Summary.csv")
#Creating combined dataframe for plotting
dat_combined <- data.frame(year = years, Ag_Land = dat_land_summary$ag_land_area_m2, Runoff_Ratio = dat_annual_summary$annual_runoff_ratio)
#Plotting runoff ratio and agricultural land sums
ggplot(data = dat_combined, aes(Ag_Land, Runoff_Ratio)) +
  geom_point() + theme_minimal() + labs(title = "Agricultural Land Cover vs Runoff Ratio (2000-2024)", x = "Agricultural Land Sums (m^2)", y = "Runoff Ratio")
#Fitting a regression model
fit <- lm(dat_combined$Runoff_Ratio ~ dat_combined$Ag_Land)
#Plotting residuals
plot(dat_combined$Ag_Land, summary(fit)$residuals, pch = 19,
     xlab="Agricultural Land Sums (m^2)", ylab="Residuals")
abline(h = 0)
#Checking normality of residuals
hist(summary(fit)$residuals, col ="red",
     main="Residual Distribution", xlab="Residuals")
#Checking with qqnorm and qqline
qqnorm(summary(fit)$residuals, pch = 19)
qqline(summary(fit)$residuals, pch = 19)
#Checking summary statistics
model_summary <- summary(fit)
print(model_summary)
