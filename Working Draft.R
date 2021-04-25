#setwd("C:\\Users\\leahc\\OneDrive - The City University of New York\\Masters\\Classes\\STA 9890 - Statistical Learning\\Final Project Git")
source("Regularization Function.R")
source("Random Forest Function.R")
load(".Rdata")
library(latex2exp)
library(gridExtra)

rid.r2 = vector(); las.r2 = vector(); elnet.r2 = vector(); rf.r2 = vector()
for (i in 1:100) {
  rid.r2 = rbind(rid.r2, regularize(0)$R2)
  las.r2 = rbind(las.r2, regularize(1)$R2)
  elnet.r2 = rbind(elnet.r2, regularize(.5)$R2)
  rf.r2 = rbind(rf.r2, RF()$R2)
}

r2.data <- cbind(rid.r2,las.r2,elnet.r2,rf.r2)
#Getting tests & trains on different sides
r2.train <- r2.data[c(1,3,5,7)]
r2.test <- r2.data[c(2,4,6,8)]

par(mfrow = c(1,2))
a=min(r2.data); b=max(r2.data) #Upper and Lower Limits for boxplots in 4b

#Part 4b ----
boxplot(x = r2.train, ylim=c(a,b), main=TeX('Training $R^2$'), names = c("Ridge","Lasso","El-net","RF"))
boxplot(x = r2.test, ylim=c(a,b), main=TeX('Test $R^2$'), names = c("Ridge","Lasso","El-net","RF"))

#Part 4c ----
par(mfrow=c(1,1))
regularize(1, cvplot=T)
regularize(.5, cvplot=T)
regularize(0, cvplot=T)

#Calculating Execution Time 
ridge.time <- regularize(0)$`KFCV Elapsed Time` #Ridge
elnet.time <- regularize(.5)$`KFCV Elapsed Time`#Elnet
lasso.time <- regularize(1)$`KFCV Elapsed Time` #Lasso
rf.time <- RF()$`KFCV Elapsed Time` #Random Forest 

#Part 4d -----
#Each of the next four code blocks is grabbing the train and test residuals for
#one realization of each of the four models
ridge.model = regularize(0)
ridge.resid.train = ridge.model$`Train Residuals`
ridge.resid.test = ridge.model$`Test Residuals`

lasso.model = regularize(1)
lasso.resid.train = lasso.model$`Train Residuals`
lasso.resid.test = lasso.model$`Test Residuals`

elnet.model = regularize(.5)
elnet.resid.train = elnet.model$`Train Residuals`
elnet.resid.test = elnet.model$`Test Residuals`

rf.model = RF()
rf.resid.train = rf.model$`Train Residuals`
rf.resid.test = rf.model$`Test Residuals`

#Combining them all into two single dataframes for plotting with boxplot()
train.resids = cbind(ridge.resid.train,lasso.resid.train,elnet.resid.train,rf.resid.train)
test.resids = cbind(ridge.resid.test,lasso.resid.test,elnet.resid.test,rf.resid.test)

par(mfrow = c(1,2))
a=min(train.resids,test.resids); b=max(train.resids,test.resids) #Setting the upper and lower limits of the boxplots
boxplot(x = train.resids, ylim=c(a,b), main='Training Residuals', names = c("Ridge","Lasso","El-net","RF"))
boxplot(x = test.resids, ylim=c(a,b), main='Test Residuals', names = c("Ridge","Lasso","El-net","RF"))

#Part 5b ----
#Finding the 5% and 95% Test R2 quantiles for each type of model
#The range of values between these quantiles constitutes a 90% interval
int.r <- quantile(r2.test$Ridge.Test.R2, probs = c(.05,.95))
int.l <- quantile(r2.test$Lasso.Test.R2, probs = c(.05,.95))
int.e <- quantile(r2.test$`El-Net.Test.R2`, probs = c(.05,.95))
int.rf <- quantile(r2.test$RF.Test.R2, probs = c(.05,.95))

times <- rbind(ridge.time, elnet.time, lasso.time, rf.time)

r2.intervals <- cbind(rbind(int.r, int.l, int.e, int.rf), times)
rownames(r2.intervals) = c("Ridge", "Lasso", "Elastic Net", "Random Forest")
colnames(r2.intervals) = c("Rsq Quantile 5%", "Rsq Quantile 95%", "Runtime")
r2.intervals

##Part 5c ----
#Present Bar plots of the coefficients
#Use elastic-net estimated coefficients to create an order largest to smallest
x=model.matrix (Salary~.,Hitters )[,-1]
y=Hitters$Salary #Response vector for ridge regression via glmnet()
p=dim(Hitters)[2]-1
rf.hitters.out  =  randomForest(Salary~., data=Hitters, mtry= floor(sqrt(p)), importance=TRUE)

cv.fit.elnet = cv.glmnet(x,y,alpha=0.5, nfolds=10)
cv.fit.lasso = cv.glmnet(x,y,alpha=1, nfolds=10)
cv.fit.ridge = cv.glmnet(x,y,alpha=0, fnolds=10)

elnet.out=glmnet(x,y,alpha=.5, lambda = cv.fit.elnet$lambda.min)
lasso.out=glmnet(x,y,alpha=1, lambda = cv.fit.lasso$lambda.min)
ridge.out=glmnet(x,y,alpha=0, lambda = cv.fit.ridge$lambda.min)


betaS.elnet             =     data.frame(c(1:p), as.vector(elnet.out$beta))
colnames(betaS.elnet)   =     c( "feature", "value")

betaS.lasso             =     data.frame(c(1:p), as.vector(lasso.out$beta))
colnames(betaS.lasso)   =     c( "feature", "value")

betaS.ridge             =     data.frame(c(1:p), as.vector(ridge.out$beta))
colnames(betaS.ridge)   =     c( "feature", "value")

betaS.rf                =     data.frame(c(1:p), as.vector(rf.hitters.out$importance))
colnames(betaS.rf)      =     c( "feature", "value")

#Use same order for Lasso, Ridge, RF, create 4x1 figure
betaS.elnet$feature     =  factor(betaS.elnet$feature, levels = betaS.elnet$feature[order(betaS.elnet$value, decreasing = TRUE)])
betaS.lasso$feature     =  factor(betaS.lasso$feature, levels = betaS.elnet$feature[order(betaS.elnet$value, decreasing = TRUE)])
betaS.ridge$feature     =  factor(betaS.ridge$feature, levels = betaS.elnet$feature[order(betaS.elnet$value, decreasing = TRUE)])

imp.rf <- importance(hitters.rf)
print(imp.rf[order(imp.rf[, 1]), ])
betaS.rf$feature        =  factor(betaS.rf$feature, levels = betaS.elnet$feature[order(betaS.elnet$value, decreasing = TRUE)])

elnetPlot =  ggplot(betaS.elnet, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")
 
lassoPlot =  ggplot(betaS.lasso, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")

ridgePlot =  ggplot(betaS.ridge, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")   

rfPlot =  ggplot(betaS.rf, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black") 

grid.arrange(elnetPlot, lassoPlot, ridgePlot, rfPlot, nrow = 4)
