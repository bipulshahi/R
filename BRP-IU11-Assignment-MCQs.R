#install.packages("R330")
library(R330)
data(wine.df)
head(wine.df)

#Q1
my.analysis <- lm(price ~ year+temp+h.rain+w.rain+h.rain*w.rain,wine.df)
summary(my.analysis)
#Q2
drop1(my.analysis,test="F")
#All Pr is < 0.05 so all variables are good , nothing to remove
#Q3
coef(my.analysis)[4]+800*coef(my.analysis)[6]
#Result is h.rain = -0.2195209

#Q4
newset <- data.frame(year=1985, temp=mean(wine.df$temp),h.rain=mean(wine.df$h.rain),w.rain=mean(wine.df$w.rain))
predict(my.analysis,newdata=newset)
#result 8.341431 
#Q5

my.analysislog <- lm(log(price) ~ year+temp+h.rain+w.rain+h.rain*w.rain,wine.df)
summary(my.analysislog)

#Q6
drop1(my.analysislog,test="F")

#Q7
my.analysislog <- update(my.analysislog,~.-h.rain:w.rain)
summary(my.analysislog)

#Q8
newset <- data.frame(year=1985, temp=mean(wine.df$temp),h.rain=mean(wine.df$h.rain),w.rain=mean(wine.df$w.rain))
exp(predict(my.analysislog,newdata=newset))

#Q9
qplot(hp, qsec, data=mtcars, geom=c("point","smooth"), method="lm")


#Q10
hist(airquality$Temp,data=airquality,breaks=10)
qplot(airquality$Temp,data=airquality,binwidth=5)
