library(dplyr)
library(stringr)

source('./helper.R')

housing = read.csv('../data/cleaned_dataset_train.csv')



housing$dtsold = as.character(housing$dtsold)

housing$yrsold = as.numeric(substr(housing$dtsold, 0, 4))

housing$monthsold = as.numeric(substr(housing$dtsold, 6, 7))

housing$dtsold = NULL

roofstyle_list = unique(housing$roofstyle)
housing$roofstyle = nested_ifelse(housing$roofstyle, roofstyle_list)


#DUMMY ALL THE THINGS
idx_list = c(89:92, 94:100, 102, 106, 115)
temp = colnames(housing)[idx_list]

for (col in temp) {
  col_list = unique(housing[[col]])
  print(col_list)
  housing[[col]] = nested_ifelse(housing[[col]], col_list)
}


write.csv(housing, 'sam_train.csv')
