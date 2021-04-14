library(ISLR)
library(leaps)
library(glmnet)

Hitters = na.omit(Hitters)

regularize <- function(a, cvplot=FALSE) {
  
# Cross-Validation ---  

  t_start = Sys.time() #Start timer
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
  #coef(ridge.mod)[,50] #Accessing the ridge regression coefficients for the 50th lambda
  
  cv.out=cv.glmnet(x.train,y.train,alpha=a,lambda = lambdas)
  t_end = Sys.time() #End timer
  CV_runtime = t_end - t_start
  
  bestlam=cv.out$lambda.min

# Plotting Cross-Validation Curve ---

  if (cvplot == TRUE) {
    #Plots the CV(10) estimate (pg. 181) for each lambda.
    if (a == 0) {plot(cv.out,sub = "10-Fold Cross Validation Curve for Ridge Regression",)}
    else if (a==.5) {plot(cv.out,sub = "10-Fold Cross Validation Curve for El-net Regression",)}
    else if (a==1) {plot(cv.out,sub = "10-Fold Cross Validation Curve for Lasso Regression",)}
    abline(v=log(bestlam), col="red")
  }

# Finding Test, Train R2 and Residuals  ---

  cv.out=glmnet(x,y,alpha=a) 
  #Ridge Regression model with unspecified lambda(?). Documentation warns
  #against supplying a single lamdba for prediction. 
  
  (y.hat.test <- predict(cv.out,s=bestlam, newx = x.test)) 
  #Predictions for model with cross validated lambda. Documentation advises to
  #supply single lambda in predict()
  (y.bar.test <- mean(y.test))
  (SSres.test <- sum((y.test-y.hat.test)^2))
  (SStot.test <- sum((y.test-y.bar.test)^2))
  (R2.test <- 1 - SSres.test/SStot.test)
  
  (y.hat.train <- predict(cv.out,s=bestlam, newx = x.train)) 
  (y.bar.train <- mean(y.train))
  (SSres.train <- sum((y.train-y.hat.train)^2))
  (SStot.train <- sum((y.train-y.bar.train)^2))
  (R2.train <- 1 - SSres.train/SStot.train)
  
  R2 = data.frame(R2.train,R2.test)
  if (a == 0) {colnames(R2) = c("Ridge.Train.R2","Ridge.Test.R2")}
  else if (a==.5) {colnames(R2) = c("El-Net.Train.R2","El-Net.Test.R2")}
  else if (a==1) {colnames(R2) = c("Lasso.Train.R2","Lasso.Test.R2")}
  
  resid.train = y.train - y.hat.train
  resid.test = y.test - y.hat.test

# Output ---

  output_list = list(`Test Residuals`=as.numeric(resid.test),
                    `Train Residuals`=as.numeric(resid.train),
                    `R2`=R2,
                    `KFCV Elapsed Time` = t_end-t_start)
  return(output_list)
}
