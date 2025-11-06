#Reading in USGS discharge data
install.packages("dataRetrieval")
library(dataRetrieval)
datD <- read.csv("Z:/omcmorrow/Project_Folder/DailyDischargeDotsero1998_2025.csv",stringsAsFactors = T)
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
print(monthly_summary, n = 329)
#Plotting monthly max and min discharge (1998-2025)
library(ggplot2)
ggplot(data = monthly_summary, aes(x = year_month)) +
  geom_line(aes(y = monthly_max), color = "blue", linewidth = 1) +
  geom_line(aes(y = monthly_min), color = "red", linewidth = 1) +
  labs(title = "Monthly Max and Min Discharge for Laguna Dam, AZ-CA (1998-2025)",
       x = "Date", y = expression(paste("Discharge ft"^"3 ","sec"^"-1"))) + theme_minimal()
