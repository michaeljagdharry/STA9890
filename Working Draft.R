setwd("C:\\Users\\leahc\\OneDrive - The City University of New York\\Masters\\Classes\\STA 9890 - Statistical Learning\\Final Project Git")
source("Regularization Function.R")


#Creating DFs to make boxplots
(R2 <- cbind(rbind(regularize(1),regularize(1)), rbind(regularize(0),regularize(0))))
R2[c(1,3,2,4)] #If desired, we can reordering the columns like this

#Calculating Execution Time 
system.time(result <- regularize(0))
