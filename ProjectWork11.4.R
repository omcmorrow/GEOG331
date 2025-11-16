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
