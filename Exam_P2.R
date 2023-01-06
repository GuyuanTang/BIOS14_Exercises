#Title: Exam_P2.R
#Date: 6 Jan 2023
#Author: Guyuan Tang
#Description: this program aims to analyse the research questions (a. mass distribution in different sex and density; 
#             b. the relationship between body mass and horn length in different sex
#Packages in use: tidyverse, sciplot, knitr,glmmTMB
################################
rm(list = ls())

library(tidyverse)
library(sciplot)
library(knitr)
library(glmmTMB)

#read the dataset and remove the NA values
dat = read.table("exam2022_part2.txt", header=T)
names(dat)
##[1] "sex"     "date"    "hornL"   "hornR"   "season"  "month"   "day"     "yr"      "daynr"  
##[10] "age"     "cohort"  "mass"    "density"

goats$cohort = as.integer(goats$cohort)

#to better analyse the data with biological meaning, we check the relationship between two horns
plot(dat$hornL, dat$hornR)
##hornL and hornR is almost the same, we generated the information into their average length hornM
goats = dat %>% mutate(hornM = (hornL + hornR)/2)

#histograms to identify the distribution of insteresed variables
hist(goats$hornM) #does not follow the pattern of normal distribution
hist(goats$mass) #approximately the normal distribution
hist(goats$age) #does not follow the pattern of normal distribution

#summarize basic description of the data

goats_sum = goats %>% summarise(horn_md = median(hornM), horn_low = quantile(hornM, 0.25), horn_up = quantile(hornM, 0.75),
                                age_m = median(age), age_low = quantile(age, 0.25), age_up = quantile(age, 0.75),
                                mass_mean = mean(mass), mass_se = se(mass), mass_sd = sd(mass),
                                sample_size = n(), cohort_m = median(cohort)) #total population
##  horn_md horn_low horn_up age_m age_low age_up mass_mean    mass_se  mass_sd sample_size cohort_m
##1     190      160     213     4       1      6  22.28166 0.08127233 5.387319        4394     1998

goats_sex = goats %>% group_by(sex) %>% summarise(horn_md = median(hornM), horn_low = quantile(hornM, 0.25), horn_up = quantile(hornM, 0.75),
                                                  age_m = median(age), age_low = quantile(age, 0.25), age_up = quantile(age, 0.75),
                                                  mass_mean = mean(mass), mass_se = se(mass), mass_sd = sd(mass),
                                                  sample_size = n(), cohort_m = median(cohort)) #grouping by sex
##  sex   horn_md horn_low horn_up age_m age_low age_up mass_mean mass_se mass_sd sample_size cohort_m
##1 F        178.     148.    198.     4       1      7      20.6  0.0915    4.05        1955     1998
##2 M        205      167.    222.     3       1      5      23.7  0.120     5.90        2439     1998

goats_den = goats %>% group_by(density) %>% summarise(horn_md = median(hornM), horn_low = quantile(hornM, 0.25), horn_up = quantile(hornM, 0.75),
                                                      age_m = median(age), age_low = quantile(age, 0.25), age_up = quantile(age, 0.75),
                                                      mass_mean = mean(mass), mass_se = se(mass), mass_sd = sd(mass),
                                                      sample_size = n(), cohort_m = median(cohort)) #grouping by density
##  density horn_md horn_low horn_up age_m age_low age_up mass_mean mass_se mass_sd sample_size cohort_m
##1 high        190      151     213     3       1      5      21.4   0.112    5.33        2258     2004
##2 low         190      163     213     4       1      6      23.2   0.114    5.29        2136     1991

#draw boxplots to identify mass distribution according to age and cohort
ggplot(goats, aes(x=factor(age), y=mass)) + geom_boxplot() + theme_classic()
ggplot(goats, aes(x=factor(cohort), y=mass)) + geom_boxplot() + theme_classic
ggplot(goats, aes(x=factor(density), y=mass)) + geom_boxplot() + theme_classic()

ggplot(goats, aes(x=sex, y=mass, fill=density)) + geom_boxplot() + theme_classic()
ggplot(goats, aes(x=sex, y=hornM, fill=density)) + geom_boxplot() + theme_classic()

#use ANOVA to test whether body mass is different in different sex and density
m_test = lm(mass ~ sex*density, data = goats)
anova(m_test)
##Analysis of Variance Table

##Response: mass
##              Df Sum Sq Mean Sq  F value  Pr(>F)    
##sex            1  10536 10536.1 408.3099 < 2e-16 ***
##density        1   3551  3551.1 137.6177 < 2e-16 ***
##sex:density    1    131   131.5   5.0943 0.02405 *  
##Residuals   4390 113280    25.8                     
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#further use ANCOVA to test whether the slopes are different in different sex
mc_test_sex = lm(mass ~ hornM * sex, data = goats)
anova(mc_test_sex)
##Analysis of Variance Table

##Response: mass
##            Df Sum Sq Mean Sq F value    Pr(>F)    
##hornM        1  64590   64590 4741.19 < 2.2e-16 ***
##sex          1   1423    1423  104.46 < 2.2e-16 ***
##hornM:sex    1   1680    1680  123.31 < 2.2e-16 ***
##Residuals 4390  59806      14                      
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#draw scatterplots
ggplot(goats, aes(x = hornM, y = mass, color = sex)) + geom_point(aes(color = sex, shape = sex), size=1.5) +
  geom_smooth(method = 'lm', se=T, linewidth = 1.5, color='black') + theme_classic() + labs(x='average horn length (mm)', y='body mass (kg)') +
  scale_color_manual(values = c('#FF99CC', '#99CCFF')) +geom_smooth(method = 'lm', se=T, linewidth = 1.5)

#linear mixed model based on the analysis above
m_fit = glmmTMB(mass ~ hornM + (1|sex) + (1|density), data = goats)
summary(m_fit)
##Family: gaussian  ( identity )
##Formula:          mass ~ hornM + (1 | sex) + (1 | density)
##Data: goats

##AIC      BIC   logLik deviance df.resid 
##23978.9  24010.8 -11984.5  23968.9     4389 

##Random effects:
  
##  Conditional model:
##  Groups   Name        Variance Std.Dev.
##   sex      (Intercept)  0.4680  0.6841  
##   density  (Intercept)  0.4638  0.6811  
##   Residual             13.6526  3.6949  
##Number of obs: 4394, groups:  sex, 2; density, 2

##Dispersion estimate for gaussian family (sigma^2): 13.7 

##Conditional model:
##            Estimate Std. Error z value Pr(>|z|)    
##(Intercept)  7.20662    0.72522    9.94   <2e-16 ***
##hornM        0.08265    0.00132   62.60   <2e-16 ***
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#fit the model by different sex
m_fit_M = glmmTMB(mass ~ hornM + (1|density), data = filter(goats, sex == 'M'))
summary(m_fit_M)
##Family: gaussian  ( identity )
##Formula:          mass ~ hornM + (1 | density)
##Data: filter(goats, sex == "M")

##AIC      BIC   logLik deviance df.resid 
##13835.4  13858.6  -6913.7  13827.4     2435 

##Random effects:
  
##Conditional model:
##Groups   Name        Variance Std.Dev.
## density  (Intercept)  0.5234  0.7235  
## Residual             16.9184  4.1132  
##Number of obs: 2439, groups:  density, 2

##Dispersion estimate for gaussian family (sigma^2): 16.9 

##Conditional model:
##            Estimate Std. Error z value Pr(>|z|)    
##(Intercept) 5.451075   0.636846    8.56   <2e-16 ***
##hornM       0.094905   0.001926   49.27   <2e-16 ***
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

m_fit_F = glmmTMB(mass ~ hornM + (1|density), data = filter(goats, sex == 'F'))
summary(m_fit_F)
##Family: gaussian  ( identity )
##Formula:          mass ~ hornM + (1 | density)
##Data: filter(goats, sex == "F")

##AIC      BIC   logLik deviance df.resid 
##9781.3   9803.6  -4886.7   9773.3     1951 

##Random effects:
  
##Conditional model:
##   Groups   Name        Variance Std.Dev.
##     density  (Intercept) 0.1826   0.4273  
##     Residual             8.6548   2.9419  
##Number of obs: 1955, groups:  density, 2

##Dispersion estimate for gaussian family (sigma^2): 8.65 

##Conditional model:
##            Estimate Std. Error z value Pr(>|z|)    
##(Intercept) 9.477368   0.413963   22.89   <2e-16 ***
##hornM       0.065630   0.001625   40.38   <2e-16 ***
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
