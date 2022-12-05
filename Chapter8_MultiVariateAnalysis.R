#Variance matrices
cm = matrix(c(0.7, 0.2, -0.3, 
              0.2, 1.2, 0.4,
              -0.3, 0.4, 0.6),
            nrow = 3)
cm == t(cm)
##translate covatiance matrix into correlation matrix
colm = matrix(rep(0,9), nrow=3)
for (i in 1:nrow(cm)) {
  for (j in 1:ncol(cm)) {
    colm[i,j] = cm[i,j]/sqrt(cm[i,i]*cm[j,j])
  }
}

#Multivariate normal distribution
library(MASS)
library(ellipse)
set.seed(1)
X = data.frame(mvrnorm(200, mu=c(0,0,0), Sigma = cm))
colnames(X) = c('z1', 'z2', 'z3')
means = c(apply(X[,1:2], 2, mean))
plot(X$z1, X$z2, las=1)
lines(ellipse(cov(X[,1:2]), centre = means))


#Eigendecomposition
eigen(cm)
##draw the eigen vector for z1 and z2
arrows(means[1], means[2],
       means[1]+eigen(cm)$vectors[1,1],
       means[2]+eigen(cm)$vectors[2,1],
       code=2, length=0.1, lwd=2, col='red')
arrows(means[1], means[2],
       means[1]+eigen(cm)$vectors[1,2],
       means[2]+eigen(cm)$vectors[2,2],
       code=2, length=0.1, lwd=2, col='blue')
##compute the proportion of variance
eva1 = eigen(cm)$values[1] / sum(eigen(cm)$values)
eva2 = eigen(cm)$values[2] / sum(eigen(cm)$values)
eva3 = eigen(cm)$values[3] / sum(eigen(cm)$values)
eva_vec = c(eva1, eva2, eva3)
#[1] 0.56138909 0.37398661 0.06462429
len_vec = c(sqrt(sum(eigen(cm)$vectors[,1]^2)), sqrt(sum(eigen(cm)$vectors[,2]^2)), sqrt(sum(eigen(cm)$vectors[,3]^2)))
#[1] 1 1 1
180/pi*acos(eigen(cm)$vectors[,1] %*% eigen(cm)$vectors[,2])
#     [,1]
#[1,]   90
##reconstruct cm
eva_vec = matrix(c(eigen(cm)$values[1],0,0,
            0,eigen(cm)$values[2],0,
            0,0,eigen(cm)$values[3]), nrow=3)
CM = eigen(cm)$vectors %*% eva_vec %*% t(eigen(cm)$vectors)
#     [,1] [,2] [,3]
#[1,]  0.7  0.2 -0.3
#[2,]  0.2  1.2  0.4
#[3,] -0.3  0.4  0.6    

#Principal Component Analysis
dim(as.matrix(X))
dim(as.matrix(eigen(cm)$vectors[,1]))
t1 = as.matrix(X) %*% eigen(cm)$vectors[,1]
t2 = as.matrix(X) %*% eigen(cm)$vectors[,2]
t3 = as.matrix(X) %*% eigen(cm)$vectors[,3]
c(var(X[,1]), var(X[,2]), var(X[,3]))
#[1] 0.7095716 1.0188924 0.6231182
c(var(t1), var(t2), var(t3))
#[1] 1.2115082 0.9550913 0.1849827
var(t1) + var(t2) + var(t3)
var(X[,1]) + var(X[,2]) + var(X[,3])

pca = princomp(X)
summary(pca)
pca$sdev^2/sum(pca$sdev^2)
pca$loadings[1:3, 1:3]
biplot(pca, col=c('grey', 'black'), cex=c(0.5,1))

blossoms = read.csv('blossoms.csv')
dat = data.frame(blossoms$GAD, blossoms$GSD, blossoms$ASD, blossoms$GA)
dat = dat[complete.cases(dat),]
pca = princomp(dat)
summary(pca)

#Principal component regression
XX = as.data.frame(scale(X))
y = 0.5*XX$z1 -0.3*XX$z2 + 0.2*XX$z3 + rnorm(200, 0, 1)
m0 = lm(y~XX$z1+XX$z2+XX$z3)
summary(m0)

pca = princomp(XX)
pc1 = pca$scores[,1]
pc2 = pca$scores[,2]
pc3 = pca$scores[,3]
m3 = lm(y~pc1+pc2+pc3)
summary(m3)

Q = pca$loadings
b = as.matrix(summary(m3)$coefficients[-1,1])
dim(Q)
dim(b)
Q %*% b

m1 = lm(y~pc1)
summary(m1)

pca$loadings[1:3, 1:3]
