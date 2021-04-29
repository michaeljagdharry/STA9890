library(readr)
library(tidyverse)
library(readxl)

WAA <- read_excel("WAA.xlsx", col_types = c("text", 
                                              +     "numeric", "numeric"))

baseball <- read_excel("baseball.xlsx")

baseball <- full_join(baseball, WAA)

baseball <- baseball %>% mutate(Wins = round(WAA + G*0.5))

baseball.df <- baseball %>% select("#Bat", "BatAge", "R/G",  "G",  "PA", "AB",
                                      "R",  "H", "2B", "3B",  "HR",      "RBI",     "SB",
                                      "CS",      "BB",   "SO",      "BA",      "OBP",     "SLG",     "OPS",
                                      "OPS+",    "TB",     "GDP",     "HBP",     "SH",      "SF",      "IBB",
                                      "LOB" ,    "#Fld",    "RA/G",    "DefEff", "GS",      "CG",      "Inn",
                                      "Ch",      "PO",      "A",       "E",       "DP",      "Fld%",    "Rtot",
                                      "Rtot/yr", "Rdrs/yr", "Rgood", "Wins" ) %>%
  na.omit()
