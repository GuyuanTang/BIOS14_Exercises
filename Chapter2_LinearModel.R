#example 1
set.seed(85)
x = rnorm(200,10,2)
y = 0.4*x + rnorm(200,0,1)
plot(x,y, las=1, #las was used to change the direction of the characters on y-axis
     xlab = 'Leaf length (mm)',
     ylab = 'Leaf width (mm)')
#fit the linear model
m = lm(y~x)
cf = summary(m)$coef #show only the coefficients
predvals = cf[1,1] + cf[2,1]*x
#draw the linear model
par(mfrow = c(1,2))
plot(x,y,las=1)
abline(m)
segments(x,y,x,predvals)
hist(residuals(m),las=1)
par(mfrow=c(1,1))#return to the default setting
summary(m)

#small exercise
slope = NULL
for (i in 1:1000) {
  position_sample = sample(1:200, replace=TRUE) #because need to pick two corresponding values, so sample the position instead
  sample_x = x[position_sample]
  sample_y = y[position_sample]
  new_m = lm(sample_y~sample_x)
  new_cf = summary(new_m)$coef
  slope[i] = new_cf[2,1]
}
sd(slope) #use the sd to represent the se, 0.03356737
se_slope = sqrt(var(slope)/length(slope))#???


cov(y,x)/var(x) #0.4333027

coefs = summary(m)$coef
(coefs[2,1]*(mean(x)+sd(x))) - (coefs[2,1]*mean(x)) #0.8606095
#coefficient of determination
cor(x,y)^2 #0.4310643
#r square
var(predvals) #0.7406487
var(predvals)/var(y) #0.4310643
#V(x)
coefs[2,1]^2*var(x) #0.7406487
#construct regression line
newx = seq(min(x), max(x), length.out=200)
predy = coefs[1,1] + coefs[2,1]*newx
plot(x,y,las=1,
     xlab = 'Leaf length (mm)',
     ylab = 'Leaf width (mm)')
lines(newx,predy)


#Exercise: How error in x- and y-variables affect the slope
set.seed(35)
x = rnorm(500,10,2)
y = 1.5*x + rnorm(500,0,1)
m = lm(y~x)
cf = summary(m)$coef
errors = seq(0.1,0.5,length.out = 10)

slopes = NULL
for (i in 1:10) {
  x_obs = x + rnorm(500, 0, errors[i])
  new_m = lm(y~x_obs)
  slopes[i] = summary(new_m)$coef[2,1]
}
plot(errors, slopes, las=1,
     xlab = 'Error standard deviation in x',
     ylab = 'Estimated slope')
#correct the estimated slopes
rel_errors = (errors^2)/var(x)
correct_slopes = slopes/(1-rel_errors)
plot(errors, slopes, las=1,
     xlab = 'Error standard deviation in x',
     ylab = 'Estimated slope',
     ylim = c(1.4,1.6))
points(errors, correct_slopes, pch=16)
segments(errors,slopes, errors, correct_slopes)
#the same procedure for y
y_slopes = NULL
for (i in 1:10) {
  y_obs = y + rnorm(500,0,errors[i])
  new_m = lm(y_obs~x)
  y_slopes[i] = summary(new_m)$coef[2,1]
}
plot(errors, y_slopes, las=1,
     xlab = 'Error standard deviation in y',
     ylab = 'Estimated slope')
#correct the y_slope
re_errors_y = (errors^2)/var(y)
correct_slopes_y = y_slopes / (1-re_errors_y)
plot(errors, y_slopes, las=1,
     xlab = 'Error standard deviation in y',
     ylab = 'Estimated slope',
     ylim = c(1.45,1.55))
points(errors, correct_slopes_y, pch=16)
segments(errors, y_slopes, errors, correct_slopes_y)


#Exercise2
birds = read.csv("bird_allometry.csv")
head(birds)
##Genus_Species Sex brain_mass body_mass
##1        Accipiter_gentilis   f   7.686143 1049.1571
##2        Accipiter_gentilis   m   7.618500  678.2833
##3           Accipiter_nisus   f   3.112797  252.1263
##4           Accipiter_nisus   m   2.637390  136.1441
##5        Accipiter_striatus   f   5.700000  520.0000
##6 Acridotheres_cristatellus   m   2.310000  122.3800

#set log(brain_size) as the predictor value x, and log(body_size) as the response value y
brain_size = birds$brain_mass
body_size = birds$body_mass
log_lm = lm(log(body_size)~log(brain_size))
summary(log_lm)$coef
##Call:
##  lm(formula = log(body_size) ~ log(brain_size))

##Residuals:
##  Min      1Q  Median      3Q     Max 
##-2.8095 -0.4592 -0.0511  0.4059  3.2380 

##Coefficients:
##  Estimate Std. Error t value
##(Intercept)      3.61059    0.02167  166.62
##log(brain_size)  1.47849    0.01900   77.82
##Pr(>|t|)    
##(Intercept)       <2e-16 ***
##  log(brain_size)   <2e-16 ***
##  ---
##  Signif. codes:  
##  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##Residual standard error: 0.669 on 1182 degrees of freedom
##Multiple R-squared:  0.8367,	Adjusted R-squared:  0.8366
##F-statistic:  6056 on 1 and 1182 DF,  p-value: < 2.2e-16

#So, the linear model would be log(body_size) = 1.47849 * log(brain_size) + 3.61059
#Turn to the original values: body_size = exp(3.61059) * brain_size^1.47849