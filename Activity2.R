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
