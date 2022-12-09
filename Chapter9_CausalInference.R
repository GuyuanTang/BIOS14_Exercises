#Cause and Correalation in Biology
plants = read.csv("E:/1Lund Lectures/BIOS14_BiologicalData/4/alpineplants.csv")

#Wrightian Path Analysis
plants = na.omit(plants)
plants = as.data.frame(scale(plants))
round(colMeans(plants),2)
round(apply(plants, 2, sd),2)

m1 = lm(data = plants, Carex.bigelowii ~ snow + min_T_winter + soil_moist)
m2a = lm(min_T_winter ~ snow, data=plants)
m2b = lm(soil_moist ~ snow, data=plants)
m2c = lm(Carex.bigelowii ~ min_T_winter + soil_moist, data=plants)

#Structural equation modelling (SEM)
library(lavaan)
library(semPlot)

mod = '
 Carex.bigelowii ~ snow+min_T_winter+soil_moist
'
lmod = sem(mod, data = plants)
summary(lmod)

par(mfrow = c(1,2))
semPaths(lmod, what = 'diagram', sizeMan = 10)
semPaths(lmod, what = 'est', sizeMan = 10, edge.label.cex = 1)

mod2 = '
min_T_winter ~ snow
soil_moist ~ snow
Carex.bigelowii ~ min_T_winter + soil_moist
'
lmod2 = sem(mod2, data=plants)
summary(lmod2, fit.measures=F)
par(mfrow=c(1,2))
semPaths(lmod2, what="diagram", sizeMan = 10)
semPaths(lmod2, what="est", sizeMan = 10, edge.label.cex = 1)

library(piecewiseSEM)
model = psem(lm(soil_moist~snow, data=plants),
             lm(min_T_winter~snow, data=plants),
             lm(Carex.bigelowii~min_T_winter+soil_moist, data=plants), data=plants)
summary(model)
plot(model)

model2 = psem(lm(Carex.bigelowii ~ snow + min_T_winter + soil_moist, data=plants))
summary(model2)
plot(model2)
