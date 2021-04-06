library(ISLR)
library(leaps)
library(glmnet)
Hitters = na.omit(Hitters)

regularize <- function(a, plot=FALSE) {
  split = sort(sample(nrow(Hitters), size = .8*nrow(Hitters), rep=F)) #Random selection of 80% of indices
  #train = Hitters[dt,] #Random subset of data
  #test = Hitters[-dt,] #Complement of random subset of data
  
  x=model.matrix (Salary~.,Hitters )[,-1]#Data matrix without intercept column for ridge regression via glmnet()
  x.train = x[split,]
  x.test = x[-split,]
  y=Hitters$Salary #Response vector for ridge regression via glmnet()
  y.train = y[split]
  y.test = y[-split]
  
  lambdas = 10^seq(6,-2,length=50); lambdas #vector of lambdas to try out
  #ridge.mod = glmnet(x,y,alpha=0,lambda=lambda_range) #Perform length(lambda_range) Ridge Regressions, each for a different lambda in lambda_range. Wow!
  #coef(ridge.mod)[,50] #Accessing the ridge regression coefficients for the 50th lambda
  #ridge.mod.train = glmnet(x[dt,],y[dt],alpha=0,lambda=lambda_range, thresh =1e-12) #Training Models 
  
  cv.out=cv.glmnet(x.train,y.train,alpha=a,lambda = lambdas); cv.out
  if (plot == TRUE) {
    plot(cv.out) #Plots the CV(10) estimate (pg. 181) for each lambda.
  }
  #Why are 19's appearing at the top?
  (bestlam=cv.out$lambda.min);abline(v=log(bestlam), col="red")
  ridge.out=glmnet(x,y,alpha=a) #Ridge Regression model with unspecified lambda(?). Documentation warns against supplying a single lamdba for prediction.
  (best_model_coef <- predict(ridge.out,type="coefficients",s=bestlam)) #Predictions for model with cross validated lambda. Documentation advises to supply single lambda in predict()
  
  #3(d)
  (y.hat.test <- predict(ridge.out,s=bestlam, newx = x.test)) #Predictions for model with cross validated lambda. Documentation advises to supply single lambda in predict()
  y.bar.test <- mean(y.test)
  (SSres.test <- sum((y.test-y.hat.test)^2))
  (SStot.test <- sum((y.test-y.bar)^2))
  (R2.test <- 1 - SSres.test/SStot.test)
  
  (y.hat.train <- predict(ridge.out,s=bestlam, newx = x.train)) #Predictions for model with cross validated lambda. Documentation advises to supply single lambda in predict()
  y.bar.train <- mean(y.train)
  (SSres.train <- sum((y.train-y.hat.train)^2))
  (SStot.train <- sum((y.train-y.bar)^2))
  (R2.train <- 1 - SSres.train/SStot.train)
  
  R2 <- data.frame(R2.train,R2.test)
  
  if (a == 0) {
    colnames(R2) = c("Ridge.Train.R2","Ridge.Test.R2")
  } else if (a==.5) {
    colnames(R2) = c("El-Net.Train.R2","El-Net.Test.R2")
  } else if (a==1) {
    colnames(R2) = c("Lasso.Train.R2","Lasso.Test.R2")
  }
  
  return(R2)
}

