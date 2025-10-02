###Testing code
##Creating a function and testing arguments
#Asserting the arguments of a function
assert <- function(statement,err.message){
  if(statement == FALSE){
    print(err.message)
  }
}
#Evaluating a false statement
assert(1 == 2, "error: unequal values")
#Evaluating a true statement
assert(2 == 2, "error: unequal values")
#Using assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")
###Question 1 looks at ATMOS41 manual
###Question 2 looks at ATMOS41 manual
###Question 3
#Read in weather station data
#Skipping first 3 rows since they are extra column info
#Specifying no header
datW <- read.csv("Z:/omcmorrow/bewkes/bewkes_weather.csv",na.strings=c("#N/A"), skip=3, header=FALSE)
#Previewing data
print(datW[1,])
#Read in sensor info
sensorInfo <- read.csv("Z:/omcmorrow/bewkes/bewkes_weather.csv",na.strings=c("#N/A"), nrows=2)
print(sensorInfo)
#Setting column names to match sensorInfo
colnames(datW) <- colnames(sensorInfo)
#Previewing data
print(datW[1,])
###Question 4
##Using packages to clean data for quality assurance
#Installing lubridate
install.packages(c("lubridate"))
#Commenting install.packages(c("lubridate")) for ease
#Using library function to load in packages to work environment
library(lubridate)
#Converting date to standardized format
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
#Calculating day of year
datW$doy <- yday(dates)
#Calculating hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#Calculating decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
datW[1,]
#Determining number of values that have missing data for each sensor observation
#Air temp
length(which(is.na(datW$air.temperature)))
#Wind speed
length(which(is.na(datW$wind.speed)))
#Precipitation
length(which(is.na(datW$precipitation)))
#Soil temp
length(which(is.na(datW$soil.moisture)))
#Soil moisture
length(which(is.na(datW$soil.temp)))
#Plotting soil moisture to visualize missing data
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
#Plotting air temp to visually compare with reasonable data
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
#Creating a new data column to convert unreliable data to NA
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)
#Checking values at the extreme ranges of the data
quantile(datW$air.tempQ1)
#Checking the days with abnormally low air temp
datW[datW$air.tempQ1 < 8,]  
#Checking the days with abnormally high air temp
datW[datW$air.tempQ1 > 33,]  
###Question 5
##Plotting precipitation and lightning strikes to determine correlation
#Normalizing lightning strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#Plotting precipitation points (semi-transparent) only when precipitation is present
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        
#Plotting lightning points (red) only when lightning is present
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)
#Filtering all values with lightning that coincides with precipitation greater than 2mm or only precipitation over 5mm
#Creating new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
#Asserting equal vector length of lightning.acvitivy and precipitation
assert <- function(statement,err.message){
  if(statement == FALSE){
    print(err.message)
  }
}
assert(length(datW$lightning.acvitivy) == length(datW$precipitation), "error: unequal length")
###Question 6
##Removing suspect measurements from overall wind speed measurements
datW$wind.speedQ1 <- ifelse(datW$wind.speed < 0, NA, datW$wind.speed)
quantile(datW$wind.speedQ1)
datW$wind.speedQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
###Question 7
##Checking that soil temp and soil moisture measurements are reliable leading up to sensor outage
###Question 8
##Creating a table with average air temp, wind speed, soil moisture, soil temp, and total precipitation for study period
###Question 9
##Plotting soil moisture, air temp, soil temp, and precipitation for entire study period
