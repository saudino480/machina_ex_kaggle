library(car)
library(dplyr)

train <- read.csv('Charlie/datafiles/train_cc_clean.csv')
test <- read.csv('Charlie/datafiles/test_cc_clean.csv')


sapply(train, is.na)
