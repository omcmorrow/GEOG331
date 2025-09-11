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
