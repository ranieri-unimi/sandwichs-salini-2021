### Regression on Employee data 

library(haven)
Dataset <- read_sav("Employee data.sav")
View(Dataset)

## Bivariate tests ###

## Does mean Salary differ by gender?
t.test(salary~gender, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=Dataset)

## Are Jobcat and Gender associated?
mytab <- xtabs(~gender+jobcat, data=Dataset)
mytab
plot(mytab, col=c("green","blue","gray"))
plot(t(mytab), col=c("green","blue","gray"))
prop.table(mytab, 1) # Row Percentages
prop.table(mytab, 2) # Col Percentages
Test <- chisq.test(mytab, correct=FALSE)
Test

## Does mean Salary differ by JobCat?
boxplot(salary ~ jobcat, data=Dataset)
library(gplots)
plotmeans(salary ~ jobcat, data=Dataset)
library(plyr)
ddply(Dataset,~jobcat,summarise,mean=mean(salary),sd=sd(salary),n=length(salary))
Dataset$jobcat<- as.factor(Dataset$jobcat)
summary(aov(salary ~ jobcat, data=Dataset))

### Correlation for continous variables
Dataset$educ<-as.numeric(Dataset$educ)
cor(Dataset[,c("salary","salbegin","educ","jobtime","prevexp")])
pairs(~salary+salbegin+educ+jobtime+prevexp,data=Dataset)

################################
### Linear models for Salary ###
################################

library(car)
Dataset<-na.omit(Dataset)
Dataset$salary<-as.numeric(Dataset$salary)
Dataset$educ<-as.numeric(Dataset$educ)
mod<-lm(salary~educ+jobtime, data=Dataset)
summary(mod)
par(mfrow=c(2,2))
plot(mod)

vif(mod) # variance inflation factors 
sqrt(vif(mod)) > 2 # problem?

## include gender, by defoult the model take the first category as reference (female in this case)
mod2<-lm(salary~educ+prevexp+gender, data=Dataset)
summary(mod2)

## use Male as reference
library(tidyverse)
Dataset$gender<-as.factor(Dataset$gender)
Dataset <- Dataset %>% mutate(gender = relevel(gender, ref = "m"))
mod2<-lm(salary~educ+prevexp+gender, data=Dataset)
summary(mod2)

## include jobcat as numerical (1,2,3)
Dataset$jobcat<- as.numeric(Dataset$jobcat)
mod3<-lm(salary~educ+prevexp+jobcat, data=Dataset)
summary(mod3)
## include jobcat as dummies
Dataset$jobcat<- as.factor(Dataset$jobcat)
mod3<-lm(salary~educ+prevexp+jobcat, data=Dataset)
summary(mod3)
### comment the difference between the two 


### model with all the variable 
library(lubridate)
Dataset$age<-(year(Sys.Date())-year(Dataset$bdate))
Dataset<-subset(Dataset,select=-c(id,bdate))
mod4<-lm(salary~., data=Dataset)
summary(mod4)
vif(mod4) # variance inflation factors 


### variable selection 
library(MASS)
# Fit the full model 
full.model <- lm(salary ~., data = Dataset)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
full.model
step.model
plot(step.model)

### log trnsformation of Y 
library(MASS)
# Fit the full model 
full.model <- lm(log(salary) ~., data = Dataset)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
full.model
step.model
plot(step.model)

