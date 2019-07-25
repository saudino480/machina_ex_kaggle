train <- read.csv('../data/clean_train.csv', stringsAsFactors = F)
test <- read.csv('../data/clean_test.csv', stringsAsFactors = F)


fe_drop =c('TotalBsmtSF', 'X1stFlrSF', 'X2ndFlrSF', 'TotRmsAbvGrd', 'LotArea', 'LotFrontage')
