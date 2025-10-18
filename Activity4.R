#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(tidyverse)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

#Reading in versicolor data
versicolor <- iris[iris$Species == "versicolor",]
#Creating lists for each regression relationship
reg_list <- list(
  c("versicolor$Sepal.Length ~ versicolor$Sepal.Width"),
  c("versicolor$Petal.Length ~ versicolor$Petal.Width"),
  c("versicolor$Sepal.Length ~ versicolor$Petal.Length"))
#Creating output list
table_list <- list()
#Regression for loop
for (i in 1:length(reg_list)) {
  model <- lm(reg_list[[i]])
  table_list[[i]] <- summary(model)
}
#Reading output
table_list
                        
#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

#Installing dplyr package
install.packages("dplyr")
library(dplyr)
#Looking at original iris dataframe
head(iris)
#Joining data to a new data frame
new_iris <- left_join(iris, height, by = "Species")
#Reading new dataframe
head(new_iris)

#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
#Installing ggplot package
install.packages("ggplot2")
library(ggplot2)
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()

#3b. make a scatter plot with ggplot and get rid of busy grid lines
#Using theme_classic to remove grid lines and background
ggplot(data = iris, aes(x= Sepal.Length, y = Sepal.Width)) + geom_point() + theme_classic()

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
ggplot(data = iris, aes(x= Sepal.Length, y = Sepal.Width)) + geom_point() + theme_classic()
  labs(title = "Iris Species Sepal Length vs. Sepal Width",
       x = "Sepal Length",
       y = "Sepal Width",
       size = "Petal Length")

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		