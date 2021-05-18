# Non parametric bootstrap confidence intervals example, test for paired sample

# wheight Before Covid
x=c(55,56,49,70,80,51,90,44,65,74,55,79,100,58,76,71,55,40,75,98)
# wheight After Covid
y=c(55,59,51,73,81,51,90,43,69,76,57,78,98,61,80,73,55,43,76,101)
# difference
diff=y-x
diff
mean(diff)
# diff follows the Nornal distribution?
hist(diff)

# confidence interval for the mean with sigma unknown (assuming normality)
x_bar=mean(diff)
n=length(diff)
SE=sd(diff)/sqrt(n)
t_alpha=qt(0.975,(length(diff)-1))   
LL=x_bar-SE*t_alpha
UL=x_bar+SE*t_alpha
LL
UL
## 
t.test(y, x, paired = TRUE, alternative = "two.sided")

## bootstrap confidence intervals 
sample(diff, replace=TRUE)
replicate(5,sample(diff, replace=TRUE))
t(replicate(5,sample(diff, replace=TRUE)))
k=10000
mysamples<-replicate(k,sample(diff, replace=TRUE))
dim(mysamples)
mymeans<-apply(mysamples, 2, mean)
hist(mymeans)
quantile(mymeans, c(0.025,0.975))

library(bootstrap)
quantile(bootstrap(diff,k,mean)$thetastar,c(0.025,0.975))




