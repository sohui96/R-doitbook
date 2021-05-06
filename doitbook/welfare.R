library(foreign)
library(readxl)

raw_welfare15 <- read.spss("./Data/Koweps_hpc10_2015_beta1.sav", to.data.frame=T)
welfare15 <- raw_welfare15

raw_welfare20 <- read.spss("./Data/Koweps_hpda15_2020_beta1.1.sav", to.data.frame=T)
welfare20 <- raw_welfare20

#save(list=ls(), file="welfare.RData")
