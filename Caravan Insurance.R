library(readr)
library(tidyverse)

Headers <- read.table("C:/Users/leahc/OneDrive - The City University of New York/Masters/Classes/STA 9890 - Statistical Learning/Final Project/Headers.csv",header = F,sep = ",")
View(Headers)
Caravan <- read.table("C:/Users/leahc/OneDrive - The City University of New York/Masters/Classes/STA 9890 - Statistical Learning/Final Project/ticdata2000.txt",
                   sep = "", 
                   header = F)

(Caravan <- rbind(Headers,Caravan))


names(Caravan) <- Caravan %>% slice(1) %>% unlist()
Caravan <- Caravan %>% slice(-1)

View(Caravan)

sum(is.na(Caravan)) #No missing values!