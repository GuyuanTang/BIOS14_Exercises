#Mean-var relations
x = seq(0,1, by=0.01)
v_b = x*(1-x)
plot(x, v_b, type='l', xlab='Probability', ylab='Theoretical variance', las=1)

logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))
x = runif(200)
logit_x = logit(x)
par(mfrow=c(2,2))
#distribution
hist(x,las=1)
hist(logit_x,las=1)

xx = seq(-5,5,0.01)
plot(xx, invlogit(xx), type='l', las=1,
     xlab = 'Logit(x)',
     ylab = 'P')
plot(x, invlogit(logit_x), las=1)

par(mfrow=c(1,1))
plot(xx, invlogit(xx), type = 'l', las=1,
     xlab = 'Logit/Probit(x)',
     ylab = 'P')
lines(xx, pnorm(xx), lty=2)
legend('topleft', legend=c("Logit", "Probit"), lty=c(1,2), text.width = 0.2)

#Poisson distribution
x = rpois(200,3)
hist(x, las=1)

#Logistic regression
rm(list=ls())
x = rnorm(200,10,3) #predictor
eta = -2 + 0.4*x + rnorm(200,0,2) #the regression model after logit
p = invlogit(eta) #return to the proportion before logit transformation
y = rbinom(200,1,p) #return to the examine value we observed
par(mfrow = c(1,3))
plot(x, eta, las=1)
plot(x, p, las=1)
plot(x,y,las=1)
#true logistic regression test
m = glm(y~x, family = binomial(link = "logit"))
summary(m)
#draw the proportion graph to better illustrate
coefs =summary(m)$coef
x_pred = seq(min(x), max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)
plot(x_pred, rbinom(1649,1,p_hat),
     xlab = 'x', ylab='y')
lines(x_pred, p_hat)
abline(h=0.5, lty=2)
abline(v=(-coefs[1,1]/coefs[2,1]), lty=2)

#Pseudo-r^2
library(MuMIn)
r.squaredGLMM(m)
##                R2m        R2c
##theoretical 0.13191280 0.13191280
##delta       0.08356573 0.08356573

#Tjur's D
y_hat = coefs[1,1] + coefs[2,1]*x
p_hat = invlogit(y_hat)
mean(p_hat[which(y==1)]) - mean(p_hat[which(y==0)])
##[1] 0.1249747

#Exercise: seed germination
rm(list=ls())
dormancy = read.csv("E:/1Lund Lectures/BIOS14_BiologicalData/5/dormancy.csv")
names(dormancy)
##[1] "pop"          "mother"       "crossID"     
##[4] "blocktray"    "timetosowing" "MCseed"      
##[7] "nseed"        "germ2"
dormancy_summary = dormancy %>% group_by(pop) %>% summarise(mean_germ = mean(germ2),
                                                            var_germ = var(germ2),
                                                            mean_ttsnow = mean(timetosowing),
                                                            mean_MC = mean(MCseed),
                                                            var_MC = var(MCseed),
                                                            mean_nseed = mean(nseed))
# pop = CC as the example
subdat = dormancy[dormancy$pop == "CC",]
germ = subdat$germ2 * subdat$nseed #number of success
notgerm = subdat$nseed - germ #number of failures
#the two following methods are the same (for more than one trial)
mod1 = glm(cbind(germ, notgerm)~subdat$timetosowing, family = binomial())
coefs1 = summary(mod1)$coef
##                       Estimate  Std. Error   z value     Pr(>|z|)
##(Intercept)         -3.58459277 0.318593043 -11.25132 2.281478e-29
##subdat$timetosowing  0.03341023 0.002825015  11.82657 2.845464e-32
exp(coefs1[2,1])
##[1] 1.033975  every unit increase in timetosowing will increase 1.034 times of proportion in germination

mod2 = glm(subdat$germ2~subdat$timetosowing, family = binomial(), weights = subdat$nseed)
coefs2 = summary(mod2)$coef
##                      Estimate  Std. Error   z value     Pr(>|z|)              
##(Intercept)         -3.58459277 0.318593043 -11.25132 2.281478e-29
##subdat$timetosowing  0.03341023 0.002825015  11.82657 2.845464e-32
exp(coefs2[2,1])
##[1] 1.033975

logLik(mod1) == logLik(mod2) # TRUE

#the duration for germination rate at 0.5
dur = -(coefs1[1,1]/coefs1[2,1]) #107
