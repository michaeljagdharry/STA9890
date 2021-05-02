source("Regularization Function Baseball.R")
source("Random Forest Function.R")
load(".Rdata")
library(latex2exp)

#Cleaning Dataset ----
library(readr)
library(tidyverse)
library(readxl)
library(gridExtra)

WAA <- read_excel("WAA.xlsx", col_types = c("text", "numeric", "numeric"))
baseball <- read_excel("baseball.xlsx")
baseball <- full_join(baseball, WAA)
baseball <- baseball %>% mutate(Wins = round(WAA + G*0.5))
baseball.df <- baseball %>% select("#Bat", "BatAge", "R/G",  "PA", "AB",
                                   "R",  "H", "2B", "3B",  "HR",      "RBI",     "SB",
                                   "CS",      "BB",   "SO",      "BA",      "OBP",     "SLG",     "OPS",
                                   "OPS+",    "TB",     "GDP",     "HBP",     "SH",      "SF",      "IBB",
                                   "LOB" ,    "#Fld",    "RA/G",    "DefEff", "GS",      "CG",      "Inn",
                                   "Ch",      "PO",      "A",       "E",       "DP",      "Fld%",    "Rtot",
                                   "Rtot/yr", "Rdrs/yr", "Rgood", "Wins" ) %>% na.omit()

baseball.df <- baseball.df %>% rename(Bat = "#Bat", Fld = "#Fld", Fld_percent = "Fld%",
                                      two_B = "2B", three_B = "3B", 
                                      OPS_plus = "OPS+", 
                                      R_by_G = "R/G", RA_by_G = "RA/G",
                                      Rtot_by_yr = "Rtot/yr", Rdrs_by_yr = "Rdrs/yr")

#Creating data structure to store R2's ----
rid.r2 = vector(); las.r2 = vector(); elnet.r2 = vector(); rf.r2 = vector()

for (i in 1:100) {
  rid.r2 = rbind(rid.r2, regularize(0)$R2)
  las.r2 = rbind(las.r2, regularize(1)$R2)
  elnet.r2 = rbind(elnet.r2, regularize(.5)$R2)
  rf.r2 = rbind(rf.r2, RF(n = 100)$R2) #For speed/debugging let ntree be small ~ 100
}

r2.data <- cbind(rid.r2,las.r2,elnet.r2,rf.r2) 
#the columns names are not automatically the r2 df names somehow, so I explicitly name them here
colnames(r2.data) = c("Ridge.Train.R2","Ridge.Test.R2",
                      "Lasso.Train.R2","Lasso.Test.R2",
                      "Elnet.Train.R2","Elnet.Test.R2",
                      "RF.Train.R2","RF.Test.R2")

#Getting tests & trains on different sides of the dataframe
r2.train <- r2.data[c(1,3,5,7)]
r2.test <- r2.data[c(2,4,6,8)]

#Part 4b ----
#Creating boxplots of training and test R^2's
par(mfrow = c(1,2))
a=min(r2.data); b=max(r2.data) #Boxplot Upper and Lower Limits
boxplot(x = r2.train, ylim=c(a,b), main=TeX('Training $R^2$'), names = c("Ridge","Lasso","El-net","RF"))
boxplot(x = r2.test, ylim=c(a,b), main=TeX('Test $R^2$'), names = c("Ridge","Lasso","El-net","RF"))

#Part 4c ----
#Creating CV plots for Ridge, Lasso, and Elnet
par(mfrow=c(1,1))
regularize(1, cvplot=T)
regularize(.5, cvplot=T)
regularize(0, cvplot=T)
ridge.cv.time <- regularize(0)$`Runtime` #Ridge
elnet.cv.time <- regularize(.5)$`Runtime`#Elnet
lasso.cv.time <- regularize(1)$`Runtime` #Lasso
cv.times <- rbind(ridge.cv.time,elnet.cv.time,lasso.cv.time)
cv.times

#Part 4d -----
#Grabbing the train and test residuals for
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

#Creating DFs of resids
train.resids = cbind(ridge.resid.train,lasso.resid.train,elnet.resid.train,rf.resid.train)
test.resids = cbind(ridge.resid.test,lasso.resid.test,elnet.resid.test,rf.resid.test)
colnames(train.resids) = c("Ridge","Lasso","El-net","RF")
colnames(test.resids) = c("Ridge","Lasso","El-net","RF")

#Boxplotting resids
par(mfrow = c(1,2))
a=min(train.resids,test.resids); b=max(train.resids,test.resids) #Setting the upper and lower limits of the boxplots
boxplot(x = train.resids, ylim=c(a,b), main='Training Residuals', names = c("Ridge","Lasso","El-net","RF"))
boxplot(x = test.resids, ylim=c(a,b), main='Test Residuals', names = c("Ridge","Lasso","El-net","RF"))

#Part 5b ----
#Finding the 5% and 95% Test R2 quantiles for each type of model
#The range of values between these quantiles constitutes a 90% interval
int.r <- quantile(r2.test$Ridge.Test.R2, probs = c(.05,.95))
int.l <- quantile(r2.test$Lasso.Test.R2, probs = c(.05,.95))
int.e <- quantile(r2.test$Elnet.Test.R2, probs = c(.05,.95))
int.rf <- quantile(r2.test$RF.Test.R2, probs = c(.05,.95))

#Calculating Execution Time 
ridge.time <- regularize(0)$`Runtime` #Ridge
elnet.time <- regularize(.5)$`Runtime`#Elnet
lasso.time <- regularize(1)$`Runtime` #Lasso
rf.time <- RF()$`Runtime` #Random Forest 

runtimes <- rbind(ridge.time, elnet.time, lasso.time, rf.time)

r2.intervals <- cbind(rbind(int.r, int.l, int.e, int.rf), runtimes)
rownames(r2.intervals) = c("Ridge", "Lasso", "Elastic Net", "Random Forest")
colnames(r2.intervals) = c("Rsq 5% Quantile", "Rsq 95% Quantile", "Runtime")
r2.intervals

##Part 5c ----
#Present Bar plots of the coefficients
#Use elastic-net estimated coefficients to create an order largest to smallest
x=model.matrix (Wins~.,baseball.df )[,-1]
y=baseball.df$Wins #Response vector for ridge regression via glmnet()
p=dim(baseball.df)[2]-1
rf.baseball.out  =  randomForest(Wins~., data=baseball.df, mtry= floor(sqrt(p)), importance=TRUE)

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

betaS.rf                =     data.frame(c(1:p), as.vector(rf.baseball.out$importance))
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
