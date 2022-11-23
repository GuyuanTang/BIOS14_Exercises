#Variance component analysis using random-effect models
set.seed(145)
x1 = rnorm(200, 10, 2)
groupmeans = rep(rnorm(10,20,4), each=20)
groupID = as.factor(rep(paste0("Group", 1:10), each=20))
y = 2 + 1.5*x1 + groupmeans + rnorm(200,0,2)
plot(x1, y, col=as.numeric(groupID), las=1)
#use the package glmmTMB to fit the model with random effects
data = data.frame(y,x1,groupID)
m = glmmTMB(y ~ 1 + (1|groupID), data = data)
summary(m)

VarCorr(m)
VarAmongGroups = attr(VarCorr(m)$cond$groupID, "stddev")^2
VarWithinGroups = attr(VarCorr(m)$cond, "sc")^2
#calculate the percent of the variance explained by groups
VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100

CV2_Among = VarAmongGroups/mean(x1)^2
CV2_Within = VarWithinGroups/mean(x1)^2
CV2_Total = CV2_Among + CV2_Within

df = data.frame(Mean = mean(x1), SD = sd(x1),
                Among = VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100,
                Within = VarWithinGroups/(VarAmongGroups+VarWithinGroups)*100,
                CV2_Among, CV2_Within, CV2_Total)
df = apply(df, 2, round, 2)

#random-intercept model
m = glmmTMB(y ~ x1 + (1|groupID), data=data)
summary(m)
