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
