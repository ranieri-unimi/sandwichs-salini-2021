# (c) Salini 2020
set.seed(123)
library("stats4")
x <- rnorm(1000, mean=5, sd=4)
log.lik <- function(mu=1, sigma=1) 
 -sum( dnorm(x, mean=mu, sd=sigma,log=TRUE) )

fit <- mle(log.lik,lower=c(0,0),method="L-BFGS-B")
fit


mean(x)
sd(x)


logLik(fit)

vcov(fit)

confint(fit)
summary(fit)


