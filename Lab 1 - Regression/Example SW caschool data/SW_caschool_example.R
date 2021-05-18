library(foreign, pos = 4)
Dataset <- read.dta("caschool.dta")
str(Dataset)
head(Dataset)
attach(Dataset)

#### Descriptives
### i create a function to do a summary including IQR and SD 
mySummary <- function(value) {
	qq <- quantile(value, na.rm = TRUE)
	mv <- c(mean(value, na.rm = TRUE), sd(value, na.rm = TRUE))
	di <- qq[4]-qq[2]
	tmp <- round(c(mv, qq[c(1, 5, 2:4)], di), 2)
	names(tmp) <- c("Mean", "SD", "Min", "Max", "Q1", 
		"Median", "Q3", "IQR")
	return(tmp)
}

apply(cbind(testscr, str), 2, mySummary)

### descriptive plots
par(mfcol = c(2, 3))
boxplot(testscr)
boxplot(str)
hist(testscr)
hist(str)
qqnorm(testscr)
qqnorm(str)

### ho to identify the univariate outlier
bxpdat<-boxplot(testscr)
bxpdat$out 
outlier_testscr<-Dataset[which(Dataset$testscr==bxpdat$out),]
outlier_testscr

apply(cbind(avginc, el_pct, meal_pct), 2, mySummary)

### boxplot
par(mfcol = c(1, 3))
boxplot(avginc, main="avginc")
boxplot(el_pct, main="el_pct")
boxplot(meal_pct, main="meal_pct")

# Scatter Plot
plot(str, testscr, main = "Scatterplot", xlab = "Student/Teacher Ratio", 
	ylab = "Standardized Test Score", pch = 15)

# add interpolation line 
abline(lm(testscr ~ str), col = "red") 

### correlation index and test
cor.test(testscr,str)

### linear model
library(car)
mod<-lm(testscr~str)
summary(mod)
confint(mod)
#### residual plots 
summary(mod$residuals)
par(mfcol = c(2, 2))
qqnorm(scale(mod$residuals))
hist(scale(mod$residuals))
boxplot(scale(mod$residuals))
plot(str,scale(mod$residuals))
plot(mod)


#### include as control variable the Percent of english learner 
mod1<-lm(testscr~str+el_pct)
summary(mod1)
confint(mod1)
cor(str,el_pct)
vif(mod1)

#### include also Percent of eligible subsidized lunch

mod2<-lm(testscr~str+el_pct+meal_pct)
summary(mod2)
confint(mod2)
vif(mod2)


#### include also the expernditure for students
mod3<-lm(testscr~str+el_pct+meal_pct+expn_stu)
summary(mod3)
anova(mod3)
confint(mod3)
vif(mod3)


#### THE COMMANDS BELOW TO OBTAIN THE RESULTS REPORTED ON CHAPTER 8 of SW book ON DUMMY VARIABLES AND NON-LINEAR MODELS (LOGARITMS)
#### use of dummy variables in regression
#### create dummy for  Students/Teachers ratio
Dataset$d1 <- factor ( with ( Dataset, ifelse ( ( str>20 ), 1 , 0 ) ) )
### create dummy for Percent of english learner  (1 if PErcent < 10, 0 otherwise)
Dataset$d2 <- factor ( with ( Dataset, ifelse ( ( el_pct>10 ), 1 , 0 ) ) )
mod4<-lm(testscr~str+d2, data=Dataset)
summary(mod4)
### interaction between two dummies
mod5<-lm(testscr~d1+d2+d1:d2, data=Dataset)
summary(mod5)
## interaction between dummy and continuous 
mod6<-lm(testscr~str+d2+d2:str, data=Dataset)
summary(mod6)

#### use of logartims in regression, we consider the average income  
Dataset$log_test<-log(Dataset$testscr)
Dataset$log_avginc<-log(Dataset$avginc)
mod7<-lm(log_test~avginc, data=Dataset)
summary(mod7)
mod8<-lm(testscr~log_avginc, data=Dataset)
summary(mod8)
mod9<-lm(log_test~log_avginc, data=Dataset)
summary(mod9)

#### use of polynomious, we consider the average income  

mod10<-lm(testscr~poly(avginc,2),data=Dataset)
mod11<-lm(testscr~poly(avginc,3),data=Dataset)

summary(mod10)
summary(mod11)

plot(avginc,testscr,pch=16)
x<-0:60
y1<-predict(mod10,list(avginc=x))
y2<-predict(mod11,list(avginc=x))
y3<-predict(mod8,list(log_avginc=log(x)))

lines(x,y1,col="red")
lines(x,y2,col="blue")
lines(x,y3,col="green")

legend(40, 650, legend=c("Quadratic","Cubic","Logaritm"),
       col=c("red", "blue","green"), lty=1:2, cex=0.8)

