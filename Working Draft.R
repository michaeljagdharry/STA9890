#setwd("C:\\Users\\leahc\\OneDrive - The City University of New York\\Masters\\Classes\\STA 9890 - Statistical Learning\\Final Project Git")
source("Regularization Function.R")
library(latex2exp)

#Creating DFs to make boxplots
regularize(.5, cvplot=T)

rid.r2 = vector(); las.r2 = vector(); elnet.r2 = vector()
for (i in 1:100) {
  rid.r2 = rbind(rid.r2, regularize(0)$R2)
  las.r2 = rbind(las.r2, regularize(1)$R2)
  elnet.r2 = rbind(elnet.r2, regularize(.5)$R2)
}

r2.data <- cbind(rid.r2,las.r2,elnet.r2)
#Getting tests & trains on different sides
r2.train <- r2.data[c(1,3,5)]
r2.test <- r2.data[c(2,4,6)]

par(mfrow = c(1,2))
a=min(r2.data); b=max(r2.data) #Upper and Lower Limits for boxplots in 4b

#Part 4b ----
boxplot(x = r2.train, ylim=c(a,b), main=TeX('Training $R^2$'), names = c("Ridge","Lasso","El-net"))
boxplot(x = r2.test, ylim=c(a,b), main=TeX('Test $R^2$'), names = c("Ridge","Lasso","El-net"))

#Part 4c ----
par(mfrow=c(1,1))
regularize(1, cvplot=T)
regularize(.5, cvplot=T)
regularize(0, cvplot=T)

#Calculating Execution Time 
regularize(0)$`KFCV Elapsed Time` #Ridge
regularize(.5)$`KFCV Elapsed Time`#Elnet
regularize(1)$`KFCV Elapsed Time` #Lasso

#Part 4d -----

ridge.resid.train = regularize(0)$`Train Residuals`
ridge.resid.test = regularize(0)$`Test Residuals`
lasso.resid.train = regularize(1)$`Train Residuals`
lasso.resid.test = regularize(1)$`Test Residuals`
elnet.resid.train = regularize(.5)$`Train Residuals`
elnet.resid.test = regularize(.5)$`Test Residuals`
train.resids = cbind(ridge.resid.train,lasso.resid.train,elnet.resid.train)
test.resids = cbind(ridge.resid.test,lasso.resid.test,elnet.resid.test)

par(mfrow = c(1,2))
a=min(train.resids,test.resids); b=max(train.resids,test.resids)
boxplot(x = train.resids, ylim=c(a,b), main=TeX('Training Residuals'), names = c("Ridge","Lasso","El-net"))
boxplot(x = test.resids, ylim=c(a,b), main=TeX('Test Residuals'), names = c("Ridge","Lasso","El-net"))
