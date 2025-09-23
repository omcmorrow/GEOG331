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
