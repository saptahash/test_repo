#!/usr/bin/env Rscript --vanilla

#if(!require(dplyr)) install.packages("dplyr")
if(!require(here)) install.packages("here")

library(here)
library(tidyverse)

here()
US_covid <- read.csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv")
write.csv(US_covid, "./auto_test/data/OxCGRT_US_latest.csv")
