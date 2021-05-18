# Non parametric bootstrap confidence intervals example, test for two-independent sample

# Number of books read in March and April by children who, 
# during the quarantine, used a parent-owned tablet 
# for school activities (Own tablet = No)
# x=c(0,3,1,1,5,6,4,5,4,2,3,1,4,5,3,7,2)
x=c(2,1,4,3,1,1,1,1,2,2,2,3,3,4,4,4,1,1,0)
# Number of books read in March and April by children who, 
# during the quarantine, used a tabelt of their exclusive use
# for school activities (Own tablet = Yes)
# y=c(1,0,2,1,5,0,1,2,3,4,1,1,2,0,3,3,4,4,0,1,1,1,0,1)
y=c(1,0,2,1,1,4,1,1,0,1,2,1,0,2,3,2,3,1,4)

# organize dataset in an other way
xx<-data.frame(books=x)
yy<-data.frame(books=y)
xx$tablet<-"No"
yy$tablet<-"Yes"
dat<-rbind(xx,yy)

# boxplot
boxplot(books~tablet, data=dat)

# Error Bar plot
library(ggpubr)
ggerrorplot(dat, x = "tablet", y = "books",
            desc_stat = "mean_ci",
            error.plot = "errorbar",            # Change error plot type
            add = "mean"                        # Add mean points
)

### ttest and confidence interval using formulas assuming normality
n1=length(x)
n2=length(y)
x_bar=mean(x)
y_bar=mean(y)
s1=sd(x)
s2=sd(y)
diff=x_bar-y_bar
s=sqrt((s1^2*(n1-1)+s2^2*(n2-1))/(n1+n2-2))
t_alpha=qt(0.975,(n1+n2-1))
t=diff/(s*sqrt(1/n1+1/n2))
t_alpha
t
LL=diff-(s*sqrt(1/n1+1/n2))*t_alpha
UL=diff+(s*sqrt(1/n1+1/n2))*t_alpha
LL
UL

#### t.test function assuming normality
t.test(x,y,var.equal = T)  # two-tail
t.test(x,y,var.equal = T, alternative = "greater") # one-tail

## bootstrap confidence intervals for the difference 
k=10000
xsamples<-replicate(k,sample(x, replace=TRUE))
ysamples<-replicate(k,sample(y, replace=TRUE))
mymeandifs<-apply(xsamples, 2, mean)-apply(ysamples, 2, mean)
hist(mymeandifs)
quantile(mymeandifs, c(0.025,0.975))

# greather
pval=1-sum(mymeandifs>0)/k
pval





