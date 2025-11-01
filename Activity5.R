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
#Plotting final average daily discharge plot with subsequent standard deviation polygon and legend additions
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#Remove gaps from axes
     axes=FALSE)#No axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#y coordinates
        col=rgb(0.392, 0.584, 0.929,.2), #Color that is semi-transparent
        border=NA#No border
)       
axis(1, seq(0,360, by=40), #Tick intervals
     lab=seq(0,360, by=40)) #Tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#Show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #Legend items
       lwd=c(2,NA),#Lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#Fill box colors
       pch=c(NA,15),#Symbols
       bty="n")#No legend border
#Starting a new plot window with standard size
dev.new(width=8,height=8)
#Creating larger plot margins
par(mai=c(1,1,1,1))
#Creating month tick marks for x axis
begin_month <- yday(ymd(paste(2017,1:12,1,sep = "-")))
#Plotting average daily discharge and standard deviation with 2017 observations
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     xlim=c(1,365),
     ylim=c(0,170),
     xaxs="i", yaxs ="i",
     axes=FALSE)
polygon(c(aveF$doy, rev(aveF$doy)),
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),
        col=rgb(0.392, 0.584, 0.929,.2),
        border=NA
)       
#Adding 2017 data line
lines(datD$doy[datD$year == 2017],datD$discharge[datD$year == 2017], lwd=2, col="red")
#Adding correct axes and legend
axis(1, at=begin_month, labels=month.abb)#Built in month abbreviations
axis(2,las = 2)
abline(h=0, xpd=FALSE)
legend("topright", c("Mean Discharge for All Years","1 Standard Deviation","2017 Discharge"),
       lwd=c(2,NA,2),
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"red"),
       pch=c(NA,15,NA),
       bty="n")

###Question 7###
#Finding what days have full 24 hours of measurements
precip_agg <- aggregate(datP$HPCP, by=list(datP$year, datP$doy), FUN="length")
colnames(precip_agg) <- c("Year", "DOY", "Observations")
print(precip_agg)
#Filtering data for only days with 24 hours of measurements
full_precip <- subset(precip_agg, Observations == 24)
print(full_precip)

#Filtering data for all other days with less than 24 hours of measurements
other_precip <- subset(precip_agg, Observations < 24)
print(other_precip)
#Plotting all discharge measurements and symbolizing days with full precipitation measurements
dev.new(width=8,height=8)
par(mai=c(1,1,1,1))
plot(datD$decYear, datD$discharge, 
     type="l",
     col="blue",
     xlab="Date", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     main="Discharge Overlaid with Days of Complete Precipitation Measurements",
     lwd=2)
points(full_precip$DOY, datD$discharge,
       col="red",
       pch=19)     
