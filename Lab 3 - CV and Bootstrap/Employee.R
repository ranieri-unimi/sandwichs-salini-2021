
#### Cross-Validation and Bootstrap - Dataset:Employee ###

library(haven)
Dataset <- read_sav("Employee data.sav")
View(Dataset)
Dataset$educ<-as.numeric(Dataset$educ)

cor(Dataset[,c("salary","educ")])
plot(Dataset$educ,Dataset$salary)

#################################
# The Bootstrap #################
#################################

library(boot)
### Standar Error Correlation Coefficient ####

boot.cor <- function(data, indices, cor.type){
  dt<-data[indices,]
  cor(dt[,1], dt[,2], method=cor.type)
}

set.seed(12345)
dat<-Dataset[,c("salary","educ")]
myBootstrap <- boot(dat, boot.cor, R=1000, cor.type='p')
myBootstrap$t0
myBootstrap$t
mean(myBootstrap$t)-myBootstrap$t0
sd(myBootstrap$t)
myBootstrap

myBootstrap <- boot(dat, boot.cor, R=1000, cor.type='s')
myBootstrap

boot.cor2 <- function(data, indices){
  dt<-data[indices,]
  c(cor(dt[,1], dt[,2], method='p'),
    cor(dt[,1], dt[,2], method='s')
  )
}

myBootstrap <- boot(dat, boot.cor2, R=1000)
myBootstrap

###############################################
#### Linear Regression Example Regression #####
###############################################

# The Validation set Approach 
set.seed(1)
train=sample(dim(Dataset)[1],dim(Dataset)[1]/2)
lm.fit=lm(salary~educ,data=Dataset,subset=train)
summary(lm.fit)
attach(Dataset)
### we print the sqrt of MSE 
sqrt(mean((salary-predict(lm.fit,Dataset))[-train]^2))
### use a polynomial model 
lm.fit2=lm(salary~poly(educ,2),data=Dataset,subset=train)
sqrt(mean((salary-predict(lm.fit2,Dataset))[-train]^2))
lm.fit3=lm(salary~poly(educ,3),data=Dataset,subset=train)
sqrt(mean((salary-predict(lm.fit3,Dataset))[-train]^2))

set.seed(2)
train=sample(dim(Dataset)[1],dim(Dataset)[1]/2)
lm.fit=lm(salary~educ,data=Dataset,subset=train)
attach(Dataset)
sqrt(mean((salary-predict(lm.fit,Dataset))[-train]^2))
lm.fit2=lm(salary~poly(educ,2),data=Dataset,subset=train)
sqrt(mean((salary-predict(lm.fit2,Dataset))[-train]^2))
lm.fit3=lm(salary~poly(educ,3),data=Dataset,subset=train)
sqrt(mean((salary-predict(lm.fit3,Dataset))[-train]^2))

####################################################################
## EXERCISE: Predict Salary using previeus experience (prevexp)#####
################ Use the validation approach #######################
####################################################################

# Leave-One-Out Cross-Validation
lm.fit=lm(salary~educ,data=Dataset)
coef(lm.fit)
### Use glm that include CV error ###
library(boot)
glm.fit=glm(salary~educ,data=Dataset)
coef(glm.fit)
cv.err=cv.glm(Dataset,glm.fit)
## print sqrt of MSE ##
sqrt(cv.err$delta[1])
## use polynomial model
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(salary~poly(educ,i),data=Dataset)
  cv.error[i]=cv.glm(Dataset,glm.fit)$delta[1]
}
sqrt(cv.error)

# k-Fold Cross-Validation

k=10 
set.seed(12)
cv.error.10=rep(0,5)
for (i in 1:5){
  glm.fit=glm(salary~poly(educ,i),data=Dataset)
  cv.error.10[i]=cv.glm(Dataset,glm.fit,K=k)$delta[1]
}
sqrt(cv.error.10)

####################################################################
## EXERCISE: Predict Salary using previeus experience (prevexp)#####
####################### Use the LOOCV ##############################
####################################################################


#####################
#### BOOK Labs ######
#####################

library(ISLR)

## Portfolio Example from Book ###

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)

## Estimating the Accuracy of a Linear Regression Model for Employee data

boot.lm=function(data,index)
  return(coef(lm(salary~educ,data=data,subset=index)))
n=dim(Dataset)[1]
boot.lm(Dataset,1:n)
set.seed(1)
boot.lm(Dataset,sample(n,n,replace=T))
boot.lm(Dataset,sample(n,n,replace=T))
myboot<-boot(Dataset,boot.lm,1000)
myboot
summary(lm(salary~educ,data=Dataset))$coef
plot(myBootstrap, index=2)

boot.ci(myboot, index=2)

####################################################################
## EXERCISE: Predict Salary using previeus experience (prevexp)###
####################### Use the bootstrap ##########################
####################################################################