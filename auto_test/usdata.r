#!/usr/bin/Rscript

if(!require(dpyl)) install.packages("dyplr")
library(dplyr)
US_covid <- read.csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv")
write.csv("./data/OxCGRT_US_latest.csv", US_covid)
