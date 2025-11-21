#Reading in USGS discharge data
install.packages("dataRetrieval")
library(dataRetrieval)
datD <- read.csv("Z:/omcmorrow/Project_Folder/DailyDischargeLagunaDam2000_2024.csv",stringsAsFactors = T)
#Reading column headers for data
head(datD)
#Formatting date in order to group by year and month
datD$Date <- as.Date(datD$time)
head(datD)
#Extracting monthly max and min discharge
library(dplyr)
library(lubridate)
monthly_summary <- datD %>%
  mutate(year_month = floor_date(Date, "month")) %>%
  group_by(year_month) %>%
  summarise(
    monthly_max = max(value, na.rm = TRUE),
    monthly_min = min(value, na.rm = TRUE)
  )
print(monthly_summary, n = 301)
#Plotting monthly max and min discharge (2000-2024)
library(ggplot2)
ggplot(data = monthly_summary, aes(x = year_month)) +
  geom_line(aes(y = monthly_max, color = "Monthly Max"), linewidth = 1) +
  geom_line(aes(y = monthly_min, color = "Monthly Min"), linewidth = 1) +
  scale_color_manual(name = "Discharge", values = c("Monthly Max" = "blue", "Monthly Min" = "red")) +
  labs(title = "Monthly Max and Min Discharge for the Colorado River Below Laguna Dam, AZ-CA (2000-2024)",
       x = "Date", y = expression(paste("Discharge ft"^"3 ","sec"^"-1"))) + theme_minimal()

##Plotting annual sums of discharge
#Removing the first day of 2025
day_to_remove <- as.Date("2025-01-01")
datDnew <- datD %>%
  filter(Date != day_to_remove)
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

##Plotting normalized river discharge
#Reading in precipitation data for the upper and lower basins
install.packages("dataRetrieval")
library(dataRetrieval)
datUp <- read.csv("Z:/omcmorrow/Project_Folder/Upper_CRB_Precip_2000-2024.csv",stringsAsFactors = T)
datLow <- read.csv("Z:/omcmorrow/Project_Folder/Lower_CRB_Precip_2000-2024.csv",stringsAsFactors = T)
#Changing upper basin to numeric format
datUp$X...Upper.Colorado.River.Basin.Precipitation <- as.character(datUp$X...Upper.Colorado.River.Basin.Precipitation)
datUp$X...Upper.Colorado.River.Basin.Precipitation <- as.numeric(datUp$X...Upper.Colorado.River.Basin.Precipitation)
Monthly_Precip <- datUp[-c(1:2),]
dat_monthly_means$precip <- Monthly_Precip
print(monthly_means)
Monthly_means <- datD %>%
  mutate(datD,
         Year = format(Date, "%Y"), Month = format(Date, "%m"))
dat_monthly_means <- Monthly_means %>%
  group_by(Year, Month) %>%
  summarise(mean_discharge_cfs = mean(value, na.rm = TRUE), .groups = 'drop')
dat_monthly_means <- head(dat_monthly_means, -1)
#Plotting monthly mean discharge vs monthly precipitation in the upper basin
scale_factor <- max(dat_monthly_means$mean_discharge_cfs) / max(dat_monthly_means$precip)

ggplot(dat_monthly_means, aes(x = Year)) +
  geom_line(aes(y = mean_discharge_cfs, color = "Discharge ft³/s)"), size = 1.1) +
  geom_bar(aes(y = precip * scale_factor, fill = "Precipitation (in)"),
           stat = "identity", alpha = 0.4) +
  scale_y_continuous(
    name = "Discharge (ft³/s)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Precipitation (in)")
  ) +
  scale_color_manual(values = c("Discharge (ft³/s)" = "black")) +
  scale_fill_manual(values = c("Precipitation (in)" = "lightblue")) +
  labs(x = "Year", title = "Discharge and Precipitation") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.title.y.right = element_text(color = "black"),
    axis.title.y.left  = element_text(color = "black")
  )

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
