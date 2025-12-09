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
