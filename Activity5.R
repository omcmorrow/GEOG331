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
#Finding frequency of discharge observations
dis_freq_table <- table(datD$discharge)
print(dis_freq_table)
#Finding frequency of precipitation observations
precip_freq_table <- table(datP$HPCP)
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
#Creating a new date column to find observation frequency
datP$new_date <- as.Date(datP$DATE, format = "%Y%m%d")
#Finding observation frequencies for all days
date_freq <- table(datP$new_date)
print(date_freq)
#Subsetting dates with all 24 precipitation measurements
full_precip <- as.Date(names(date_freq[date_freq == 24]), format = "%Y-%m-%d")
print(full_precip)
#Creating dataframe for only days with 24 hours of measurements
dat_full_precip <- datP[datP$new_date %in% as.Date(full_precip),]
print(dat_full_precip)
#Formatting correct date for discharge
datD$Date <- datesD
#Creating a binary variable column based on full precipitation measurements
datD$full_precip <- datD$Date %in% full_precip
#Plotting all discharge measurements
dev.new(width=8,height=8)
par(mai=c(1,1,1,1))
plot(datD$Date, datD$discharge,
     type="h",
     col="blue",
     xlab="Date", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     main="Discharge Overlaid with Days of Complete Precipitation Measurements",
     lwd=2)
#Adding in rug tick marks on x axis to show full precipitation days
rug(datD$Date[datD$full_precip], 
     side=1,
     col="red",
     lwd=2)
legend("topleft", c("Discharge for All Years","Days with Complete Precipitation Measurements"),
       lwd=c(2,2),
       col=c("blue","red"),
       bty="n")

###Question 8###
#Subsetting discharge and precipitation for September 5 and 6, 2011
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]
#Looking at minimum flow
min(hydroD$discharge)
#Using floor to round down the minimum of discharge range
yl <- floor(min(hydroD$discharge))-1
#Using ceiling to round up the maximum of discharge range
yh <- ceiling(max(hydroD$discharge))+1
#Finding the range for precipitation
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#Scaling precipitation to fit on plot
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl
#Plotting discharge
par(mai=c(1,1,1,1))
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#Adding precipitation bars
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}
#Subsetting discharge and precipitation for January 12, 2011
hydro1 <- datD[datD$doy == 12 & datD$year == 2011,]
hydro2 <- datP[datP$doy == 12 & datP$year == 2011,]
#Repeating steps for ranges
yl <- floor(min(hydro1$discharge))-1
yh <- ceiling(max(hydro1$discharge))+1
pl <- 0
pm <-  ceiling(max(hydro2$HPCP))+.5
hydro2$pscale <- (((yh-yl)/(pm-pl)) * hydro2$HPCP) + yl
#New plot
par(mai=c(1,1,1,1))
plot(hydro1$decDay,
     hydro1$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     main="Hydrograph for January 12, 2011",
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#Adding precipitation bars
for(i in 1:nrow(hydro2)){
  polygon(c(hydro2$decDay[i]-0.017,hydro2$decDay[i]-0.017,
            hydro2$decDay[i]+0.017,hydro2$decDay[i]+0.017),
          c(yl,hydro2$pscale[i],hydro2$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}
legend("topleft", c("Discharge", "Precipitation"),
       lwd=c(2,NA),
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),
       pch=c(NA,15),
       bty="n")

###Question 9###
library(ggplot2)
#Turning year into factor data
datD$yearPlot <- as.factor(datD$year)
#Making a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()
#Making a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()
#Defining ranges of seasons for 2016
datD$seasons2016 <- with(datD, ifelse(doy %in% 1:81 | doy %in% 356:366, "Winter", 
                                 ifelse(doy %in% 82:172, "Spring",
                                        ifelse(doy %in% 173:265, "Summer", "Fall"))))
#Defining ranges of seasons for 2017
datD$seasons2017 <- with(datD, ifelse(doy %in% 1:80 | doy %in% 355:365, "Winter", 
                                 ifelse(doy %in% 81:171, "Spring",
                                        ifelse(doy %in% 172:264, "Summer", "Fall"))))
#Subsetting data for 2016 and 2017
dat2016 <- subset(datD, year == 2016)
dat2017 <- subset(datD, year ==2017)
#Violin plot for 2016
ggplot(dat2016, aes(x = seasons2016, y = discharge)) + 
  geom_violin(fill = "lightblue", color = "black") +
  labs(title = "Seasonal Discharge for 2016", x = "Season", y = expression(paste("Discharge ft"^"3 ","sec"^"-1"))) +
  theme_minimal()
#Violin plot for 2017
ggplot(dat2017, aes(x = seasons2017, y = discharge)) + 
  geom_violin(fill = "lightblue", color = "black") +
  labs(title = "Seasonal Discharge for 2017", x = "Season", y = expression(paste("Discharge ft"^"3 ","sec"^"-1"))) +
  theme_minimal()
