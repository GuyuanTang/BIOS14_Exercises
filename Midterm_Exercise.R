#Title: Midterm_Exercise.R
#Date: 30 Nov 2022
#Author: Guyuan Tang
#Description: this program aims to analyse the research questions (a. whether there is difference in GA among
#             populations; b.the relationship between GA and the distances including GAD, GSD and ASD) 
#             based on the dataset blossoms.csv
#Packages in use: tidyverse, sciplot, plyr, knitr, glmmTMB
################################

library(tidyverse)
library(sciplot)
library(plyr)
library(knitr)
library(glmmTMB)
#read the data from the csv file
blossoms = read.csv("blossoms.csv")
names(blossoms)
## [1] "pop"   "patch" "ASD"   "GAD"   "GSD"   "LBL"   "LBW"   "UBL"   "UBW"   "GW"    "GA" 

#make descriptive summary of the dataset
sample_size = aggregate(blossoms$GA, by=list(pop=blossoms$pop), length)
##  pop  x
##1  S1 30
##2 S11 34
##3 S12 25
##4  S2 14
##5 S20 18
##6 S27 22
##7  S7 25
##8  S8 20
##9  S9 17
#total sample size: 205

popstats = ddply(blossoms, .(pop), summarize,
                GADm = mean(GAD, na.rm=T),
                GADsd = sd(GAD, na.rm=T),
                GADse = se(GAD, na.rm=T),
                GSDm = mean(GSD, na.rm=T),
                GSDsd = sd(GSD, na.rm=T),
                GSDse = se(GSD, na.rm=T),
                ASDm = mean(ASD, na.rm=T),
                ASDsd = sd(ASD, na.rm=T),
                ASDse = se(ASD, na.rm=T),
                GAm = mean(GA, na.rm=T),
                GAsd = sd(GA, na.rm=T),
                GAse = se(GA, na.rm=T))
popstats[,-1] = round(popstats[,-1], 2)
kable(popstats)
#########
##  |pop | GADm| GADsd| GADse| GSDm| GSDsd| GSDse| ASDm| ASDsd| ASDse|   GAm| GAsd| GAse|
##  |:---|----:|-----:|-----:|----:|-----:|-----:|----:|-----:|-----:|-----:|----:|----:|
##  |S1  | 4.73|  1.08|  0.20| 4.75|  0.73|  0.13| 2.56|  1.20|  0.22| 19.79| 3.52| 0.64|
##  |S11 | 5.27|  0.80|  0.14| 4.57|  0.63|  0.11| 3.16|  0.89|  0.15| 16.95| 4.73| 0.81|
##  |S12 | 4.65|  0.70|  0.14| 5.02|  0.90|  0.18| 2.66|  0.84|  0.17| 21.75| 4.68| 0.94|
##  |S2  | 4.66|  0.61|  0.16| 5.01|  0.60|  0.16| 3.87|  1.03|  0.27| 25.14| 5.56| 1.49|
##  |S20 | 6.38|  0.91|  0.23| 4.91|  0.52|  0.13| 6.32|  1.71|  0.43| 34.62| 7.72| 1.82|
##  |S27 | 4.39|  0.61|  0.13| 5.14|  0.62|  0.13| 2.98|  1.08|  0.23| 23.90| 3.69| 0.79|
##  |S7  | 4.60|  0.79|  0.16| 5.08|  0.65|  0.13| 3.92|  1.06|  0.21| 23.44| 7.03| 1.41|
##  |S8  | 5.72|  0.95|  0.22| 4.89|  0.64|  0.14| 4.52|  1.20|  0.27| 29.56| 8.86| 1.98|
##  |S9  | 5.42|  0.66|  0.17| 4.57|  0.74|  0.18| 4.05|  0.90|  0.22| 23.16| 5.23| 1.27|
#######

#summary of the interested variables on the total population
blossoms_sum = blossoms %>% summarise(GSDm = mean(GSD, na.rm=T), GSDse = se(GSD, na.rm=T),
                                      GSD_sd = sd(GSD, na.rm=T),
                                      GADm = mean(GAD, na.rm=T), GADse = se(GAD, na.rm=T),
                                      GAD_sd = sd(GAD, na.rm=T),
                                      ASDm = mean(ASD, na.rm=T), ASDse = se(ASD, na.rm=T),
                                      ASD_sd = sd(ASD, na.rm=T),
                                      GAm = mean(GA, na.rm=T), GAse = se(GA, na.rm=T),
                                      GA_sd = sd(GA, na.rm=T))
blossoms_sum = round(blossoms_sum, 2)
##  GSDm GSDse GSD_sd GADm GADse GAD_sd ASDm ASDse ASD_sd   GAm GAse GA_sd
##1 4.87  0.05    0.7 5.03  0.07   0.98 3.59   0.1   1.49 23.35 0.52  7.47



#histograms to see the distribution of the interested variables: GA, ASD, GAD, and GSD
hist(blossoms$GA)
hist(blossoms$ASD)
hist(blossoms$GAD)
hist(blossoms$GSD)
#all are approximately close to the pattern of Gaussian normal distribution


#boxplot to show the difference among population on GA
ggplot(data = blossoms, aes(x=pop, y=GA)) +
  geom_boxplot(fill="grey") +
  labs(x='Population', y='Gland area (mm^2)') +
  geom_hline(yintercept = 23.35, linetype = 2) +
  theme_classic()

#anova table to identify the significant differences on GAm among populations
m1 = lm(GA~pop, data=blossoms)
m1_aov = anova(m1)
##Analysis of Variance Table
##Response: GA
##           Df Sum Sq Mean Sq F value    Pr(>F)    
##pop         8 4944.1  618.01  18.813 < 2.2e-16 ***
##Residuals 196 6438.6   32.85                      
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 

#use Tukey test to find the populations that caused the significant differences
TukeyHSD(aov(GA~pop, data = blossoms))
#  Tukey multiple comparisons of means
##95% family-wise confidence level

##Fit: aov(formula = GA ~ pop, data = blossoms)

##$pop
##             diff          lwr        upr     p adj
##S11-S1   -2.8400597  -7.34350546  1.6633861 0.5604117
##S12-S1    1.9600200  -2.90859853  6.8286385 0.9407247
##S2-S1     5.3489193  -0.47019287 11.1680314 0.0991550
##S20-S1   14.8252272   9.46505463 20.1853998 0.0000000 *
##S27-S1    4.1108414  -0.93559428  9.1572770 0.2127585
##S7-S1     3.6462420  -1.22237653  8.5148605 0.3182985
##S8-S1     9.7675550   4.57759020 14.9575198 0.0000006 *
##S9-S1     3.3701653  -2.08765053  8.8279811 0.5890474
##S12-S11   4.8000797   0.06342662  9.5367328 0.0443042 *
##S2-S11    8.1889790   2.47981948 13.8981385 0.0003927 *
##S20-S11  17.6652869  12.42468713 22.9058867 0.0000000 *
##S27-S11   6.9509011   2.03165837 11.8701438 0.0005185 *
##S7-S11    6.4863017   1.74964862 11.2229548 0.0009041 *
##S8-S11   12.6076147   7.54123823 17.6739912 0.0000000 *
##S9-S11    6.2102250   0.86979528 11.5506547 0.0100511 *
##S2-S12    3.3888993  -2.61251097  9.3903095 0.7009596
##S20-S12  12.8652072   7.30766199 18.4227525 0.0000000 *
##S27-S12   2.1508214  -3.10478245  7.4064252 0.9350460
##S7-S12    1.6862220  -3.39888421  6.7713282 0.9814109
##S8-S12    7.8075350   2.41396537 13.2011046 0.0003301 *
##S9-S12    1.4101453  -4.24163427  7.0619249 0.9972271
##S20-S2    9.4763079   3.06967630 15.8829396 0.0002168 *
##S27-S2   -1.2380779  -7.38462096  4.9084651 0.9994046
##S7-S2    -1.7026773  -7.70408754  4.2987330 0.9932901
##S8-S2     4.4186357  -1.84628354 10.6835550 0.4018347
##S9-S2    -1.9787540  -8.46730022  4.5097922 0.9891558
##S27-S20 -10.7143859 -16.42834904 -5.0004227 0.0000006 *
##S7-S20  -11.1789852 -16.73653045 -5.6214400 0.0000001 *
##S8-S20   -5.0576722 -10.89878488  0.7834404 0.1490050
##S9-S20  -11.4550619 -17.53540864 -5.3747152 0.0000005 *
##S7-S27   -0.4645994  -5.72020318  4.7910045 0.9999989
##S8-S27    5.6567136   0.10210658 11.2113207 0.0423288 *
##S9-S27   -0.7406761  -6.54633525  5.0649831 0.9999812
##S8-S7     6.1213130   0.72774337 11.5148826 0.0135011 *
##S9-S7    -0.2760767  -5.92785627  5.3757029 1.0000000
##S9-S8    -6.3973897 -12.32823285 -0.4665466 0.0238706 *


#ggplots to test whether there are differences among populations in the linear regression outcome
ggplot(data=blossoms, aes(x=ASD, y=GA)) + geom_point(color = 'darkgrey') + 
  facet_grid(cols = vars(pop)) + geom_smooth(method = 'lm', se=FALSE, color = 'black') +
  labs(x='Anther-stigma distance (mm)', y='Gland area (mm^2)') #the population may have random effect on the regression model
ggplot(data=blossoms, aes(x=GAD, y=GA)) + geom_point(color = 'darkgrey') + 
  facet_grid(cols = vars(pop)) + geom_smooth(method = 'lm', se=FALSE, color = 'darkblue') +
  labs(x='Gland-anther distance (mm)', y='Gland area (mm^2)')
ggplot(data=blossoms, aes(x=GSD, y=GA)) + geom_point(color = 'darkgrey') + 
  facet_grid(cols = vars(pop)) + geom_smooth(method = 'lm', se=FALSE, color = 'darkred') +
  labs(x='Gland-stigma distance (mm)', y='Gland area (mm^2)')

#GLMM regression model (consider the population as the random effect)
m_fit = glmmTMB(GA ~ ASD+GSD+GAD+(1|pop), data=blossoms) #with the lowest AIC
m_fit_sum = summary(m_fit)
##Family: gaussian  ( identity )
##Formula:          GA ~ ASD + GSD + GAD + (1 | pop)
##Data: blossoms_new

##   AIC      BIC   logLik deviance df.resid 
##1218.2   1238.0   -603.1   1206.2      194 

##Random effects:

##  Conditional model:
##Groups   Name        Variance Std.Dev.
##pop      (Intercept)  7.861   2.804   
##Residual             22.114   4.703   
##Number of obs: 200, groups:  pop, 9

##Dispersion estimate for gaussian family (sigma^2): 22.1 

##Conditional model:
##               Estimate Std. Error z value Pr(>|z|)    
##  (Intercept)  -3.6024     3.1247  -1.153    0.249    
##  ASD           1.4248     0.3113   4.577 4.73e-06 ***
##  GSD           2.7510     0.5313   5.178 2.25e-07 ***
##  GAD           1.7810     0.4462   3.992 6.55e-05 ***
##  ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#calculate the 95% CI for slopes of ASD, GSD and GAD
confint(m_fit, 'ASD', level = 0.95)
##        2.5 %   97.5 % Estimate
##ASD 0.8146417 2.035048 1.424845
confint(m_fit, 'GSD', level = 0.95)
##       2.5 %   97.5 % Estimate
##GSD 1.709589 3.792316 2.750952
confint(m_fit, 'GAD', level = 0.95)
##        2.5 %   97.5 % Estimate
##GAD 0.9065592 2.655468 1.781014
