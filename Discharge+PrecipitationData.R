#Reading in USGS discharge data
install.packages("dataRetrieval")
library(dataRetrieval)
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
datDnew$value <- datDnew$value * multiplier
#Extracting monthly max and min discharge (2000-2024)
library(dplyr)
library(lubridate)
monthly_summary <- datDnew %>%
  mutate(year_month = floor_date(Date, "month")) %>%
  group_by(year_month) %>%
  summarise(
    monthly_max = max(value, na.rm = TRUE),
    monthly_min = min(value, na.rm = TRUE)
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
dat_monthly_means$totalprecip_ins <- dat_monthly_means$totalprecip_ins * multiplier2
dat_monthly_data <- dat_monthly_means %>%
  rename(total_precip_m = totalprecip_ins)
#Creating precip value column to be joined to discharge
monthly_precip <- dat_total_precip[-c(1,2),]
#Calculating monthly means for river discharge (2000-2024)
monthly_means <- datDnew %>%
  mutate(datDnew,
         Year = format(Date, "%Y"), Month = format(Date, "%m"))
dat_monthly_means <- monthly_means %>%
  group_by(Year, Month) %>%
  summarise(mean_discharge_cfs = mean(value, na.rm = TRUE), .groups = 'drop')
print(dat_monthly_means, n = 300)
#Joining total monthly precip to monthly discharge means
dat_monthly_means$totalprecip_ins <- monthly_precip
print(dat_monthly_means)
#Calculating runoff ratio (total discharge / total precipitation) to normalize discharge
dat_monthly_means$runoffratio <- dat_monthly_means$mean_discharge_cfs / dat_monthly_means$totalprecip_ins
print(dat_monthly_means)


##Plotting annual sums of discharge
#Calculating annual discharge sums
annual_sums <- datDnew %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%                  
  summarise(Total_Discharge_cfs = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(Year = as.numeric(Year))
#Plotting
ggplot(annual_sums, aes(x = Year, y = Total_Discharge_cfs)) +
  geom_line(color = "blue", linewidth = 1.5) + 
  geom_point(color = "blue") +
  labs(title = "Annual Total Discharge Trend for the Colorado River Below Laguna Dam, AZ-CA (2000-2024)",
       x = "Year",
       y = expression(paste("Discharge ft"^"3 ","sec"^"-1"))) +
  theme_bw() +
  scale_x_continuous(breaks = seq(min(annual_sums$Year), max(annual_sums$Year), by = 5))


#Plotting monthly mean discharge vs monthly precipitation in the upper basin
scale_factor <- max(dat_monthly_means$mean_discharge_cfs) / max(dat_monthly_means$precip)

par(mar = c(5.1, 4.1, 4.1, 4.2))
plot(dat_monthly_means$Year, dat_monthly_means$precip, type = "h", col = "lightgray", lwd = 20,
     ylab = "Precipitation (in)", xlab = "Year")

par(new = TRUE)  # allow overlaying a second plot

plot(dat_monthly_means$Year, dat_monthly_means$mean_discharge_cfs, type = "h", col = "blue" , lwd = 2,
     axes = FALSE, xlab = "", ylab = "")
axis(side = 4)  # add right axis
mtext("Discharge (ft³/s)", side = 4, line = 3)
legend("topleft",
       legend = c("Precipitation (in)", "Discharge (ft³/s)"),
       col = c("lightgray", "blue"),
       lty = c(1,1),
       bty = "n",
       cex = 0.7
       )
title(main = "Colorado River Discharge Below Laguna Dam, AZ-CA vs Annual Precipitation of the Upper CRB (2000-2024)")
