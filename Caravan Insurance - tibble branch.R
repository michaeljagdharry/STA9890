library(readr)
library(tidyverse)

Headers <- read_csv("https://raw.githubusercontent.com/michaeljagdharry/STA9890/main/Headers.csv") ## Successfully made header tibble from Github
View(Headers)
Caravan <- read_delim("https://raw.githubusercontent.com/michaeljagdharry/STA9890/main/ticdata2000.txt", 
                      "\t", escape_double = FALSE, col_names = FALSE, 
                      trim_ws = TRUE) ## It worked after many attempts, Pull of tibble data
View(Caravan)

Caravan <- rbind(Headers,Caravan)  #  Isn't binding column headers
Caravan <- bind_rows(Headers, Caravan) ## This doesn't work, ends up returning NA for all values


names(Caravan) <- Caravan %>% slice(1) %>% unlist()
Caravan <- Caravan %>% slice(-1)

View(Caravan)

sum(is.na(Caravan)) #No missing values!