#Load Libraries
library(lattice)
library(psych)
library(gmodels)
#Encode Categorical Variables
MultiRegDataset$smoker=factor(MultiRegDataset$smoker,
                              levels=c('yes','no'),
                              labels=c(1,2))
d<-MultiRegDataset

#Plotting all variables against each other to see relation
plot(d)

#Displaying Dataset
MultiRegDataset

#Basic statistics of dataset
describe(MultiRegDataset)

#Correlation Analysis of Dataset
cor(MultiRegDataset$age,MultiRegDataset$expenses)

cor(MultiRegDataset$bmi,MultiRegDataset$expenses)


cor(MultiRegDataset$children,MultiRegDataset$expenses)

#Calculating Skewness
library(moments)

#calculate skewness

skewness(MultiRegDataset$expenses)


#Histogram of expenses
x=MultiRegDataset$expenses
h<-hist(x, breaks=10, col="red", xlab="Expenses",
        main="Expenses Distribution for the Dataset ")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)




#T-test that mean for expenses equal to 10000
#null hypothesis Ho= mean of expenses=10000
#Alternative hypothesis Ha= mean of expenses!=10000
#One Sample t-test
t.test(MultiRegDataset$expenses, mu=10000)


#Linear Regression using smoker as independent variable, and expenses as dependent variable

#Build Linear Model

simple.fit<-lm(expenses~smoker, data=MultiRegDataset) 
LinearModel<-simple.fit 

#Summary of Key Statistics of the Model
summary(LinearModel)

#Multiple linear regression model
library(MASS)
# Fit the full model
full.model <- lm(expenses~., data = MultiRegDataset)
summary(full.model)

