log(1.1/1)
-log(1/1.1)

#simulating data from statistical distribution
x = rnorm(100,mean=5,sd=1)
mean(x) #4.915
sd(x) #0.8942
hist(x,las=1,main = "")

#bootstrapping
set.seed(1)
x = rnorm(50,10,2)
se_x = sqrt(var(x)/length(x)) #0.23515
sample(x,replace=TRUE)
out = NULL
for (i in 1:1000) {
  sample = sample(x,replace = TRUE)
  out[i] = mean(sample)
}
hist(out, las=1, main="")
sd(out) #0.23078
#95% CI
quantile(out, c(0.025,0.975)) #9.76,10.624
mean(x) - 1.96*se_x #9.739
mean(x) + 1.96*se_x #10.624

#95% CI for CV of X
cv_x = sqrt(var(x))/mean(x) #0.163004
<<<<<<< Updated upstream
cv_x - 1.96*se_x #-0.2978972 wrong should not use the SE, because SE is for the mean
=======
cv_x - 1.96*se_x #-0.2978972 wrong
>>>>>>> Stashed changes
cv_x + 1.96*se_x #0.6239054 wrong

#Exercise1
cv_out = NULL
for (i in 1:1000) {
  sample = sample(x, replace=TRUE)
  cv_out[i] = sqrt(var(sample))/mean(sample)
}
quantile(cv_out, c(0.025,0.975)) #0.1227,0.20026

#Exercise2
x = rnorm(100,10,2)
out_sd = NULL
out_CV = NULL
for (i in 1:1000) {
  sample = sample(x, replace = TRUE)
  out_sd[i] = sd(log(sample))
  out_CV[i] = sqrt(var(sample))/mean(sample)
}
plot(out_CV,out_sd)
fit = lm(out_sd~out_CV)
abline(fit, col='red')
