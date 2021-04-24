library(randomForest)
Hitters = na.omit(Hitters)

RF <- function() {
  t_start = Sys.time() #Start timer
  train = sort(sample(nrow(Hitters), size = .8*nrow(Hitters), rep=F)) #Random selection of 80% of indices
  
  p=dim(Hitters)[2]-1
  rf.hitters  =  randomForest(Salary~., data=Hitters, subset = train, mtry= floor(sqrt(p)), importance=TRUE)
  t_end = Sys.time() #End timer
  CV_runtime = t_end - t_start
  
  x.train = Hitters[train,]
  x.test = Hitters[-train,]
  y.train      = Hitters[train, "Salary"]
  y.test       = Hitters[-train, "Salary"]
 
  #rf.cv <- rfcv(trainx = Hitters[train,-19], trainy = Hitters[train, "Salary"],cv.fold = 10,step = .8) #step is the amount by which we should reduce the number of predictor after each iteration
  #rf.cv$error.cv #Cross Validation Error Estimates for each submodel 
  #with(rf.cv, plot(n.var, error.cv, log="x", type="o", lwd=2)) #Cross Validation Curve
  y.hat.test   = predict(rf.hitters, newdata = x.test)
  (y.bar.test <- mean(y.test))
  (SSres.test <- sum((y.test-y.hat.test)^2))
  (SStot.test <- sum((y.test-y.bar.test)^2))
  (R2.test <- 1 - SSres.test/SStot.test)
  
  (y.hat.train <- predict(rf.hitters, newx = x.train)) 
  (y.bar.train <- mean(y.train))
  (SSres.train <- sum((y.train-y.hat.train)^2))
  (SStot.train <- sum((y.train-y.bar.train)^2))
  (R2.train <- 1 - SSres.train/SStot.train)
  
  R2 = data.frame(R2.train,R2.test)
  colnames(R2) = c("RF.Train.R2","RF.Test.R2")
  
  resid.train = y.train - y.hat.train
  resid.test = y.test - y.hat.test
  
  output_list = list(`Test Residuals`=as.numeric(resid.test),
                     `Train Residuals`=as.numeric(resid.train),
                     `R2`=R2,
                     `KFCV Elapsed Time` = t_end-t_start)
  
  return(output_list)
}
