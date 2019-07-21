library(tidyverse)
library(dplyr)
library(ggrepel)
library(stringr)

source('./helper.R')

# This assumes that you are in a named folder in the root directory of the
# GitHub project.
housing = read.csv("../data/train.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
#housing = read.csv("../data/test.csv", stringsAsFactors = FALSE)

cleanS = SamsCleaner(housing)
cleanMD = MikeDClean(housing)
cleanC = CharlieClean(housing)
cleanMJ = MikeJrClean(housing)

full_df = cbind(cleanS, cleanMD, cleanC, cleanMJ)
#write.csv(clean, "clean_data_1_20.csv")
write.csv(clean, "clean_test_1_20.csv")