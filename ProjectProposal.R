#Reading in USGS discharge data
install.packages("dataRetrieval")
library(dataRetrieval)
datW <- read.csv("Z:/omcmorrow/Project_Folder/DailyDischargeDotsero1998_2025.csv",stringsAsFactors = T)
#Reading column headers for data
head(datW)
#Plotting daily discharge data from 1998 to 2025 using ggplot
install.packages("ggplot2")
library(ggplot2)
ggplot(data = datW, aes(x = time, y = value)) + scale_y_log10() + geom_point() +
  labs(title = "Daily Discharge for Desoto, CO (1998-2025)",
     x = "Day of Year",
     y = "Discharge (cubic ft/s)")
  
  