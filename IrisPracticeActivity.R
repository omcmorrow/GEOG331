### OM 10/7/25
#Subset the virginica species
flower <- iris[iris$Species == "virginica",]
#Making a scatter plot
plot(flower$Sepal.Length, flower$Petal.Length, pch = 19,
     xlab="Sepal Length", ylab="Petal Length",
     main="Iris virginica")
#Fitting a regression model
fit <- lm(flower$Petal.Length ~ flower$Sepal.Length)
#Plotting residuals
plot(flower$Sepal.Length, summary(fit)$residuals, pch = 19,
     xlab="Sepal Length", ylab="Residuals")
abline(h = 0)
#Checking normality of residuals
hist(summary(fit)$residuals, col ="red",
     main="Residual Distribution", xlab="Residuals")
#Checking with qqnorm and qqline
qqnorm(summary(fit)$residuals, pch = 19)
qqline(summary(fit)$residuals, pch = 19)
#Using Shapiro Wilks test
shapiro.test(summary(fit)$residuals)