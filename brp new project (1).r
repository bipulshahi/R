install.packages("ggplot2")
install.packages("car")
install.packages("caret")
install.packages("corrplot")
library(ggplot2)
library(car)
library(caret)
library(corrplot)

#Loading data
data(mtcars)  

# Looking at variables
str(mtcars)
head(mtcars)
summary(mtcars)

#Data Prepration
mtcars$am   = as.factor(mtcars$am)
mtcars$cyl  = as.factor(mtcars$cyl)
mtcars$vs   = as.factor(mtcars$vs)
mtcars$gear = as.factor(mtcars$gear)

#Dropping dependent variable for calculating Multicollinearity
mtcars_a = subset(mtcars, select = -c(mpg))
mtcars_a
mtcars
#Identifying numeric variables
numericData <- mtcars_a[sapply(mtcars_a, is.numeric)]
numericData
#Calculating Correlation
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)

# Visualize Correlation Matrix
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7,
         tl.col = rgb(0, 0, 0))

# Checking Variables that are highly correlated
highlyCorrelated = findCorrelation(descrCor, cutoff=0.7)

#Identifying Variable Names of Highly Correlated Variables
highlyCorCol = colnames(numericData)[highlyCorrelated]

#Print highly correlated attributes
highlyCorCol

#Remove highly correlated variables and create a new dataset
dat3 = mtcars[, -which(colnames(mtcars) %in% highlyCorCol)]
dim(dat3)

#ACTUAL MODEL CREATION
#Build Linear Regression Model
fit = lm(mpg ~ ., data=dat3)

#Check Model Performance
summary(fit)

#Extracting Coefficients
summary(fit)$coeff
 

par(mfrow=c(2,2))
plot(fit)

#Check summary of model and ANOVA table
summary(fit)
 

#CALCULATING MODEL PERFORMANCE
#Extracting R-squared value
summary(fit)$r.squared
#Extracting Adjusted R-squared value
summary(fit)$adj.r.squared

##prediction 
pred3 = predict(fit,dat3)
finaldata3 = cbind(mtcars,pred3)
print(head(subset(finaldata3, select = c(mpg,pred3))))
 