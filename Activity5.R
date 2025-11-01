###Question 2###
#Loading lubridate
library(lubridate)
#Reading in streamflow data
datH <- read.csv("Z:/data/hw5_data/stream_flow_data.csv", na.strings = c("Eqp"))
#Reading column headers
head(datH)
#Reading in precipitation data
datP <- read.csv("Z:/data/hw5_data/2049867.csv")
#Reading column headers
head(datP)
#Subsetting for reliable measurements indicated by USGS data quality
datD <- datH[datH$discharge.flag == "A",]
#Converting date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#Day of year
datD$doy <- yday(datesD)
#Calculating year
datD$year <- year(datesD)
#Defining the time
timesD <- hm(datD$time)
#Defining time for precipitation data
dateP <- ymd_hm(datP$DATE)
#Day of year
datP$doy <- yday(dateP)
#Calculating year
datP$year <- year(dateP)
##Formatting date and time in decimal days/day of year
#Converting time from string format
#Decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#Full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#Calculating decimal year including leap years
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#Calculating times for precipitation data
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#Full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#Calculating decimal year including leap years
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))
#Reading in decimal year data to check date formatting
print(datD)

###Question 3###
#Plotting discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#Finding number of observations in streamflow data
str(datD)
#Finding number of observations in precipitation data
str(datP)
#Creating discharge vector
discharge <- (datD$discharge)
#Creating precipitation vector
precip <- (datP$HPCP)
#Finding frequency of discharge observations
dis_freq_table <- table(discharge)
print(dis_freq_table)
#Finding frequency of precipitation observations
precip_freq_table <- table(precip)
print(precip_freq_table)

###Question 4###
#Documentation on expression function
?expression

###Question 5###
#Calculating average daily discharge
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
#Formatting respective columns
colnames(aveF) <- c("doy","dailyAve")
#Calculating standard deviation of daily discharge
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
#Formatting respective columns
colnames(sdF) <- c("doy","dailySD")
#Starting a new plot window with standard size
dev.new(width=8,height=8)
#Creating larger plot margins
par(mai=c(1,1,1,1))
#Plotting average daily discharge
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

