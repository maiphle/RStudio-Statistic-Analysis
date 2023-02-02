
#--------------------------------------------------------------------------------------------------
# Import data
#--------------------------------------------------------------------------------------------------
setwd("/cloud/project")
dat <- read.csv("tvmarketing.csv", header = T)

#--------------------------------------------------------------------------------------------------
# Checking for normality 
#--------------------------------------------------------------------------------------------------

#Summary statistics
sd(dat$Sales)
mean(dat$Sales)
summary(dat$Sales)

#Check normality of 'sales' variable
boxplot(dat$Sales,main = "Boxplot of Sales", xlab = "Sales")
hist(dat$Sales,main = "Histogram of Sales", xlab = "Sales", ylab = "Frequency")
qqnorm(dat$Sales, main = "Normal Prob Plot for Sales")
qqline(dat$Sale)

#Check normality of 'TV' variable
boxplot(dat$TV,main = "Boxplot of TV Marketing Budget", xlab = "TV Marketing Budget")
hist(dat$TV,main = "Histogram of TV Marketing Budget", xlab = "TV Marketing Budget", ylab = "Frequency")
qqnorm(dat$TV, main = "Normal Prob Plot for TV Marketing Budget")
qqline(dat$TV)

plot(dat$Sales, dat$TV, main="Scatterplot of TV Marketing Budget vs Sales",xlab="Sales", ylab="TV Marketing Budget")

#Perform Shapiro-Wilkes test on Sales data
shapiro.test(dat$Sales)

#--------------------------------------------------------------------------------------------------
# Correlation 
#--------------------------------------------------------------------------------------------------

#Set x & y for easier use 
x <- dat$TV
y <- dat$Sales

#Compute Pearson's correlation
cor(x,y)
cor.test(x,y)

#Spearman's correlation coefficient
cor(x,y, method = "spearman")

#Kendall's tau
cor(x,y, method = "kendall")

#--------------------------------------------------------------------------------------------------
# Linear Regression 
#--------------------------------------------------------------------------------------------------

#model
newModel3 <- lm(Sales ~ TV, data = dat)

#If there is missing data, there are two options:
newModel4 <- lm(y ~ x, na.action = na.fail)	#Model fails if there are missing values
newModel5 <- lm(y ~ x, na.action = na.omit)	#Model excludes missing data & fits models

#View regression results
summary(newModel3) #

#Pull put coefficients (slope & intercept)
newModel3$coeff
b1 <- newModel3$coeff[2]
b0 <- newModel3$coeff[1]

#Look at predicted values
predict(newModel3) #show each observation in data set
b1*dat$TV + b0

#Look at residuals
resid(newModel3)

#Look model
plot(newModel3)



