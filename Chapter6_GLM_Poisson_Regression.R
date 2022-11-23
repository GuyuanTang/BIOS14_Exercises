#Poisson distribution
x = rpois(200,3)
hist(x, las=1)
#different parameter lambda
x = seq(0, 20, 1)
y = dpois(x, lambda=1)
plot(x,y, type="b", las=1, xlab="k", ylab="P(x=k)", pch=16, col=1)
points(x, dpois(x, lambda=3), type="b", pch=16, col=2)
points(x, dpois(x, lambda=10), type="b", pch=16, col=3)
legend("topright", col=1:3, pch=16,
       legend=c(expression(paste(lambda, " = 1")),
                expression(paste(lambda, " = 3")),
                expression(paste(lambda, " = 10"))), cex=0.7)
#normalization through log-transformation
x = rnorm(200, 10, 3)
eta = -2 + 0.2*x
y = ceiling(exp(eta + rpois(200, 0.3)))
par(mfrow = c(1,2))
plot(x, eta, las=1)
plot(x,y, las=1)
#Poisson regression model
m = glm(y~x, family = "poisson")
summary(m)
##             Estimate Std. Error   z value     Pr(>|z|)
##(Intercept) -1.258556 0.18014594 -6.986313 2.822039e-12
##x            0.211295 0.01491533 14.166299 1.481023e-45
par(mfrow = c(1,1))
plot(x,y, las=1, col='darkgrey', pch=16)
xx = seq(min(x), max(x), 0.01)
y_hat = predict(m, newdata = list(x=xx), type = "response", se.fit = T)
lines(xx, y_hat$fit)
lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)
polygon(c(xx, rev(xx)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col=rgb(0,1,0,.5), border=FALSE)
r.squaredGLMM(m)
##             R2m       R2c
##delta     0.5152153 0.5152153
##lognormal 0.5536683 0.5536683
##trigamma  0.4704572 0.4704572
1 - (m$deviance/m$null.deviance)
##[1] 0.3321988

#negative binomial
library(MASS)
m = glm.nb(y~x)
summary(m)

#Exercise: bee distribution
dat = read.csv("E:/1Lund Lectures/BIOS14_BiologicalData/6/Eulaema.csv")
head(dat)
bee_num = dat$Eulaema_nigrita
hist(bee_num, las=1)
m = glm(bee_num~dat$Tseason, family = "poisson")
coefs = summary(m)$coef
plot(dat$Tseason, bee_num, xlab='altitude', ylab='bee numbers')
y_hat = coefs[1,1] + coefs[2,1]*dat$Tseason
y_rev = exp(y_hat)
lines(dat$MAP, y_rev, col='green')

#Hurdle models
x = rnorm(200, 10, 3)
eta = -2 + 0.2*x
y = ceiling(exp(eta + rpois(200, 0.3)))
y[15:55] = 0 #set some 0 count
y1 = ((y>1)*1) #turn y1 into 0/1 variable
m1 = glm(y1~x, family = "binomial"(link = "logit"))

y2 = y
y2[which(y==0)] = NA
m2 = glm(y2~x, family = "poisson", na=na.exclude)

coefs1 = summary(m1)$coef
coefs2 = summary(m2)$coef
y_hat1 = coefs1[1,1] + coefs1[2,1]*x
y_hat2 = coefs2[1,1] + coefs2[2,1]*x
invlogit = function(x) 1/(1+exp(-x))
y_pred = invlogit(y_hat1)*exp(y_hat2)

par(mfrow=c(1,3))
plot(x, invlogit(y_hat1), las=1)
plot(x, exp(y_hat2), las=1)
plot(x, y_pred, las=1)
