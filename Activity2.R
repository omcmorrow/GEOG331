#Vector of tree heights in meters
heights <- c(30,41,20,22)
#Converting to cm
heights_cm <- heights*100
heights_cm
heights[1]
heights[2:3]
#Matrix function practice
Mat <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat
Mat.bycol <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol
Mat.bycol[1,2]
Mat.bycol[1,]
Mat.bycol[,2]
#Reading in weather station file
datW <- read.csv("Z:/omcmorrow/noaa_weather/2011124.csv",stringsAsFactors = T)
#More info on dataframe
str(datW)
#Change date format
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#Create date column only including years
#Indicate that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))
#Numeric data example vector
num_example <- c(5.4,7.9,10,3.6,15)
num_example
#Integer data example vector
int_example <- c(1,2,3,4,5)
int_example
#Character data example vector
char_example <- c("this","is","indeed","a","vector")
char_example
#Factor data example vector
fact_example <- factor(c("yes","no","no","yes","no"))
fact_example
#All unique site names
unique(datW$NAME)
#Mean maximum temp for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
#Average daily temp calculation
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
#Finding mean across all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp
#Converting level to number for factor data type
datW$siteN <- as.numeric(datW$NAME)
#Making histogram for the first site in levels
Aberdeen_hist <- hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "grey50",
     border = "white")
help(hist)
help(paste)
#Plotting histograms of four distinct sites
#Aberdeen histogram with mean and standard deviation
par(mfrow=c(2,2))
Aberdeen_hist <- hist(datW$TAVE[datW$siteN == 1],
                      freq=FALSE,
                      main = paste(levels(datW$NAME)[1]),
                      xlab = "Average daily temperature (degrees C)",
                      ylab = "Relative frequency",
                      col = "grey50",
                      border = "white")
abline(v=mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      col = "tomato3",
      lwd = 3)
abline(v=mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#Livermore histogram with mean and standard deviation
Livermore_hist <- hist(datW$TAVE[datW$siteN == 2],
                      freq=FALSE,
                      main = paste(levels(datW$NAME)[2]),
                      xlab = "Average daily temperature (degrees C)",
                      ylab = "Relative frequency",
                      col = "grey50",
                      border = "white")
abline(v=mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE),
       col = "blue",
       lwd = 3)
abline(v=mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE),
       col = "blue",
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "blue", 
       lty = 3,
       lwd = 3)
#Mandan Experiment Station histogram with mean and standard deviation
MES_hist <- hist(datW$TAVE[datW$siteN == 3],
                       freq=FALSE,
                       main = paste(levels(datW$NAME)[3]),
                       xlab = "Average daily temperature (degrees C)",
                       ylab = "Relative frequency",
                       col = "grey50",
                       border = "white")
abline(v=mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE),
       col = "yellow",
       lwd = 3)
abline(v=mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE),
       col = "yellow",
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "yellow", 
       lty = 3,
       lwd = 3)
#Mormon Flat histogram with mean and standard deviation
Mormon_Flat_hist <- hist(datW$TAVE[datW$siteN == 4],
                 freq=FALSE,
                 main = paste(levels(datW$NAME)[4]),
                 xlab = "Average daily temperature (degrees C)",
                 ylab = "Relative frequency",
                 col = "grey50",
                 border = "white")
abline(v=mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE),
       col = "green",
       lwd = 3)
abline(v=mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE),
       col = "green",
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "green", 
       lty = 3,
       lwd = 3)
#Aberdeen histogram with normal distribution
Aberdeen_hist <- hist(datW$TAVE[datW$siteN == 1],
                      freq=FALSE,
                      main = paste(levels(datW$NAME)[1]),
                      xlab = "Average daily temperature (degrees C)",
                      ylab = "Relative frequency",
                      col = "grey50",
                      border = "white")
x.plot <- seq(-10,30, length.out = 100)
y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
y.scaled <- (max(Aberdeen_hist$density)/max(y.plot)) * y.plot
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)
#Working with probability calculations and normal distribution
help(dnorm)
#Probability of temps below 0 degrees C at site 1
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#Probability of temps below 5 degrees C at site 1
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#Probability of temps between 0 and 5 degrees C at site 1
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
                                                        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#Probability of temps above 20 degrees C at site 1
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#High temp that occurs less than 10 percent of time at site 1
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#Aberdeen high temp probability with plus 4 degree C mean
1 - pnorm(18.51026,
          mean(14.43227),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#Aberdeen precipitation histogram
Aberdeen_hist <- hist(datW$PRCP[datW$siteN == 1],
                      freq=FALSE,
                      main = paste(levels(datW$NAME)[1]),
                      xlab = "Daily precipitation",
                      ylab = "Relative frequency",
                      col = "grey50",
                      border = "white")
