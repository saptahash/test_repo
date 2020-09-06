#!/usr/bin/Rscript

if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

US_covid <- read.csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv")
write.csv("./data/OxCGRT_US_latest.csv", US_covid)
