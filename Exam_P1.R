#Title: Exam_P1.R
#Date: 2 Jan 2023
#Author: Guyuan Tang
#Description: this program aims to analyse the research questions (a. whether there is difference in GA between 2 species; 
#             b.whether environment (D/W) play a role in GA) based on the dataset
#Packages in use: tidyverse, sciplot, knitr
################################

library(tidyverse)
library(sciplot)
library(knitr)

#read the data from the csv file
dat = read.csv("exam2022_part1.csv")
names(dat)
#[1] "table" "pop"   "sp"    "treat" "plant" "GAD"   "ASD"   "GSD"   "GA"    "UBW"   "UBL"   "LBL"  "LBW" 

#remove the NA values
blossom = na.omit(dat)

#summarize the GA in different species and treatments
blossom_sum = blossom %>% summarise(GAm = mean(GA), GAse = se(GA),
                                    GA_sd = sd(GA), sample_size = n())
##       GAm       GAse     GA_sd sample_size
##1 3.545027 0.03661593 0.6985875         364
blossom_dw = blossom %>% group_by(treat) %>% summarise(GAm = mean(GA), GAse = se(GA),
                                                     GA_sd = sd(GA), sample_size = n())
##  treat   GAm   GAse GA_sd sample_size
##1 D      3.29 0.0435 0.579         177
##2 W      3.78 0.0527 0.721         187
blossom_sp = blossom %>% group_by(sp) %>% summarise(GAm = mean(GA), GAse = se(GA),
                                                        GA_sd = sd(GA), sample_size = n())
##  sp      GAm   GAse GA_sd sample_size
##1 L      3.93 0.0451 0.644         204
##2 S      3.05 0.0301 0.381         160

###################

#histograms to see the distribution of GA in different species
blossom_s = filter(blossom, sp=="S")
hist(blossom_s$GA)
blossom_l = filter(blossom, sp=='L')
hist(blossom_l$GA)
#both are approximately close to the pattern of normal distribution
#apply one-way ANOVA to test whether there is difference in GA between species
m_sp = lm(GA ~ sp-1, data = blossom)
anova(m_sp)
##Analysis of Variance Table

##Response: GA
##            Df Sum Sq Mean Sq F value    Pr(>F)    
##sp           2 4644.4  2322.2    7837 < 2.2e-16 ***
##Residuals  362  107.3     0.3                      
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

###############

#summarize the GA in different species under different treatments
means = tapply(blossom$GA, list(blossom$sp, blossom$treat), mean)
sds = tapply(blossom$GA, list(blossom$sp, blossom$treat), sd)
ses = tapply(blossom$GA,
             list(blossom$sp, blossom$treat),
             function(x) sd(x)/sqrt(sum(!is.na(x))))
means
##         D        W
##L 3.595619 4.291010
##S 2.856111 3.209091
sds
##          D         W
##L 0.4984436 0.5862132
##S 0.3708861 0.3108521
ses
##           D          W
##L 0.04864311 0.05891664
##S 0.04370935 0.03313695

#boxplot to show the difference clearly
ggplot(blossom, aes(x=sp, y=GA, fill=treat)) +
  geom_boxplot() +
  labs(x='Species', y='Gland area (mm^2)') +
  scale_fill_manual(name = "Treatments",
                    labels = c("Dry", "Wet"),
                    values = c('grey', 'white')) +
  theme_classic()

#apply two-way ANOVA to test the difference in GA between species under different treatments
m_dw_sp = lm(GA ~ sp*treat, data = blossom)
anova(m_dw_sp)
##Analysis of Variance Table

##Response: GA
##           Df Sum Sq Mean Sq F value    Pr(>F)    
##sp          1 69.889  69.889 323.858 < 2.2e-16 ***
##treat       1 26.962  26.962 124.939 < 2.2e-16 ***
##sp:treat    1  2.613   2.613  12.106 0.0005641 ***
##Residuals 360 77.689   0.216                      
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


