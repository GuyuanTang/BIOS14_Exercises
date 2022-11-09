# 1.
set.seed(100)
groups = as.factor(rep(c("Small", "Medium", "Large"), each = 50))
x = c(rnorm(50,10,3), rnorm(50,13,3), rnorm(50,14,3))
plot(groups,x,las=1, xlab = "")
## Extra exercise
plot(as.numeric(groups) + rnorm(150,0,0.03), x, las=1,
     xlab="", type='p', col='grey', xlim=c(0.5,3.75),
     xaxt='n', yaxt='n')
axis(1,1:3,labels = levels(groups))
means = tapply(x,groups,mean)
points(1:3, means, pch =16, col='black')
par(new=T)
plot(groups, x, at=c(1.3, 2.3, 3.3), boxwex=0.3, 
     xlim=c(0.5, 3.75), xaxt="n", yaxt="n",
     las=1, xlab="", ylab="")

# 2.
m = lm(x~groups)
anova(m)
SS_T = 319.97 + 1200.43
SS_T / (150-1)
var(x)
summary(m)

# 3.
groups = factor(groups, levels=c("Small", "Medium", "Large"))
m = lm(x~groups-1)
summary(m)$coef
confint(m)

#Data exercise
dat = read.csv("E:/1Lund Lectures/BIOS14_BiologicalData/3/butterflies.csv")
names(dat)
larvals = factor(dat$LarvalHost) #remember to factor()
maternals = factor(dat$MaternalHost)
#on development time
DT = dat$DevelopmentTime
m_larval= lm(DT~larvals-1)
anova(m_larval)
##Analysis of Variance Table

##Response: DT
##           Df Sum Sq Mean Sq F value    Pr(>F)    
##larvals     2 180687   90344   16403 < 2.2e-16 ***
##Residuals 285   1570       6                      
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(m_larval)$coef
##                Estimate Std. Error  t value      Pr(>|t|)
##larvalsBarbarea 22.48333   0.174925 128.5313 2.329018e-254
##larvalsBerteroa 28.95327   0.226880 127.6149 1.728689e-253

plot(larvals, DT,las=1, xlab = "", ylab="DevelopmentTime", main="Larvalhost plant on DevelopmentTime")

m_maternal= lm(DT~maternals-1)
anova(m_maternal)
##Analysis of Variance Table

##Response: DT
##          Df Sum Sq Mean Sq F value    Pr(>F)    
##maternals   2 178502   89251  6773.6 < 2.2e-16 ***
##Residuals 285   3755      13                      
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(m_maternal)$coef
##                  Estimate Std. Error  t value      Pr(>|t|)
##maternalsBarbarea 23.55414  0.2896995 81.30543 3.179444e-199
##maternalsBerteroa 26.51538  0.3183654 83.28602 4.398311e-202

plot(maternals, DT,las=1, xlab = "", ylab="DevelopmentTime", main="Maternalhost plant on DevelopmentTime")

m_lav_mat = lm(DT~larvals*maternals)
anova(m_lav_mat)
##Analysis of Variance Table

##Response: DT
##                   Df  Sum Sq Mean Sq F value    Pr(>F)    
##larvals             1 2809.15 2809.15  801.36 < 2.2e-16 ***
##maternals           1  496.87  496.87  141.74 < 2.2e-16 ***
##larvals:maternals   1   80.80   80.80   23.05 2.561e-06 ***
##Residuals         283  992.05    3.51                      
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
summary(m_lav_mat)$coef
##                                   Estimate Std. Error    t value      Pr(>|t|)
##(Intercept)                       21.696078  0.1853842 117.033059 1.046514e-241
##larvalsBerteroa                    5.303922  0.3132140  16.933859  6.623805e-45
##maternalsBerteroa                  1.816742  0.2816188   6.451068  4.795007e-10
##larvalsBerteroa:maternalsBerteroa  2.202489  0.4587566   4.800996  2.561159e-06

interaction.plot(larvals,maternals,DT,type = "b",
                 col=c("red","blue"),pch=c(16,18))
