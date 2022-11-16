##Multiple regression
set.seed(187)
x1 = rnorm(200,10,2)
x2 = 0.5*x1 + rnorm(200,0,4)
y = 0.7*x1 + 2.2*x2 + rnorm(200,0,4)
m = lm(y ~ x1+x2)
coefs = summary(m)$coef

#total variance explained by the model
y_hat = coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*x2
var(y_hat) #85.4221
var(y_hat)/var(y) #0.8682827

#the variance explained by x1
y_hat1 = coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*mean(x2)
var(y_hat1) #1.608668
var(y_hat1)/var(y) #0.01635
#var(y_hat1) = b^2 * var(x1)
coefs[2,1]^2*var(x1) #1.608668

#the variance explained by x2
y_hat2 = coefs[1,1] + coefs[2,1]*mean(x1) + coefs[3,1]*x2
var(y_hat2) #79.293
var(y_hat2)/var(y) #0.8059861
#var(y_hat2) = b^2 * var(x2)
coefs[3,1]^2*var(x2) #79.29333

#var(a+b) = var(a) + var(b) + 2cov(a,b)
var(y_hat1) + var(y_hat2) + 2*cov(y_hat1, y_hat2) #85.4221
var(y_hat) #85.4221

#matrix notation V(y_hat) = (β_hat)T*S*β_hat
t(coefs[2:3,1]) %*% cov(cbind(x1,x2)) %*% coefs[2:3,1]
##       [,1]
##[1,] 85.4221

#standardize predictor variables by variance (unit the same as the sd)
x1_z = (x1 -mean(x1))/sd(x1)
x2_z = (x2 -mean(x2))/sd(x2)
m = lm(y ~ x1_z+x2_z)
summary(m)

#transformation with mean-scaling (unit the same as the mean)
x1_m = (x1 - mean(x1)) / mean(x1)
x2_m = (x2 - mean(x2)) / mean(x2)
summary(lm(y ~ x1_m + x2_m))


####################################
##Multicollinearity 多重共线性
m1 = lm(x1~x2)
summary(m1)
r2 = summary(m1)$r.squared
1/(1-r2) #1.041714


##################################
##Data exercise: multiple regression and variable selection
rm(list = ls())
plants = read.csv(file = "E:/1Lund Lectures/BIOS14_BiologicalData/4/alpineplants.csv")
#Carex.bigelowii
#row86 has NA, remove row 86
plants = plants[-86,]
mean(plants$Carex.bigelowii) #1.363158
var(plants$Carex.bigelowii) #3.145969
m_C = lm(plants$Carex.bigelowii ~ plants$mean_T_winter+plants$mean_T_summer+plants$max_T_winter+plants$min_T_winter
         +plants$max_T_summer+plants$min_T_summer+plants$light+plants$snow+plants$soil_moist+plants$altitude)
coefs_C = summary(m_C)$coef #mean_T_summer, min_T_summer, altitude are statistically significant
m_C_1 = lm(plants$Carex.bigelowii ~ plants$mean_T_summer+plants$min_T_summer+plants$altitude)
coefs_C_1 = summary(m_C_1)$coef
##                        Estimate Std. Error   t value    Pr(>|t|)
##(Intercept)          -45.96033941 17.2987709 -2.656856 0.009314715
##plants$mean_T_summer   0.85326742  0.3650485  2.337409 0.021611153
##plants$min_T_summer   -0.53475793  0.2000582 -2.673011 0.008909354
##plants$altitude        0.03464461  0.0115296  3.004841 0.003432665
TS1 = lm(plants$mean_T_summer~plants$min_T_summer)
r2 = summary(TS1)$r.squared
1/(1-r2) #1.0022<3, pass the multicollinearity test

meanTS_z = (plants$mean_T_summer - mean(plants$mean_T_summer))/sd(plants$mean_T_summer)
minTS_z = (plants$min_T_summer - mean(plants$min_T_summer))/sd(plants$min_T_summer)
alti_z = (plants$altitude - mean(plants$altitude))/sd(plants$altitude)
mC1_z = lm(plants$Carex.bigelowii ~ meanTS_z+minTS_z+alti_z)
coefs_mC1_z = summary(mC1_z)$coef
##               Estimate Std. Error   t value     Pr(>|t|)
##(Intercept)  1.3631579  0.1611713  8.457818 4.285731e-13
##meanTS_z     0.6913688  0.2957843  2.337409 2.161115e-02
##minTS_z     -0.4814634  0.1801202 -2.673011 8.909354e-03
##alti_z       0.9280705  0.3088585  3.004841 3.432665e-03


############################
##ANCOVA
rm(list=ls())
set.seed(12)
x = rnorm(200,50,5)
gr = factor(c(rep("Male",100), rep("Female",100)))
y = -2 + 1.5*x + rnorm(200,0,5)
y[101:200] = 2 + 0.95*x[101:200] + rnorm(100,0,6)
plot(x,y,pch=c(1,16)[as.numeric(gr)],las=1)

m = lm(y~ x*gr)
anova(m)
summary(m)

m2 = lm(y ~ gr + x:gr -1)
summary(m2)

logLik(m) #'log Lik.' -615.7634 (df=5)
logLik(m2) #'log Lik.' -615.7634 (df=5)

#################
##Data exercise: Interpreting linear-model analyses
rm(list=ls())
blossoms = read.csv(file = "E:/1Lund Lectures/BIOS14_BiologicalData/4/blossoms.csv")
names(blossoms)
tapply(blossoms$UBW, blossoms$pop, mean, na.rm=T) #remove the NA value in the data

library(plyr)
library(knitr)
popstats = ddply(blossoms, .(pop), summarize,
                 LBWm = mean(LBW, na.rm=T),
                 LBWsd = sd(LBW, na.rm=T),
                 GSDm = mean(GSD, na.rm=T),
                 GSDsd = sd(GSD, na.rm=T),
                 ASDm = mean(ASD, na.rm=T),
                 ASDsd = sd(ASD, na.rm=T))
popstats[,-1] = round(popstats[,-1],2) #round all the values except for the popname


