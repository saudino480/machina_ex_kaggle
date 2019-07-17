# This script will clean the training set for columns 21 - 40.  For columns missing data,
# imputation was used based on the classifcation of MCAR, MAR, and MNAR.  There were only
# MCAR missing for this range.  The rest of the 'NA' values were used for classifications.
# Those values were replaced by 'None' for character entries.  Only one value was imputed 
# by discretion, and the rest were imputedbased on common sense.  For example, if a
# basement is non-existent, then it should not be described as unfinished.

# This script may need to be updated for the testing data set.

train <- read.csv('../data/train.csv', stringsAsFactors = F)



###############################
# just focusing on columns 21-40...

#library(mice)

# look at what is missing..
#md.pattern(train[colnames(train)[21:40]], rotate.names = T)

# look at only the missing data
# create mask for columns with missing data:
#colmask = c('MasVnrType', 'MasVnrArea', 'BsmtQual', 'BsmtCond', 'BsmtFinType1', 'BsmtExposure', 'BsmtFinType2')
#md.pattern(train[colmask], rotate.names = T)

#View(train[colmask])

# BsmtQual, BsmtCond, BsmtFinType1, BsmtExposure, BsmtFintType2 all have 37 missing.
# This is probably because there is no basement in these homes.  I will impute 'None'
# for all of these, but I will be left with one MCAR in each of BsmtExposure and BsmtFinType2


# I tried to automate this, and it was not working out.  The problem was that I didn't
# want to replace all the NAs in BsmtExposure and BsmtFinType2, because they appear to be
# MCAR, as opposed to the 37 other NA values.  That being said, I only wanted to replace
# via the index from either BsmtQual or BsmtCond.

idx_Bsmt = which(is.na(train$BsmtQual))
train$BsmtQual[idx_Bsmt] = 'None'
train$BsmtCond[idx_Bsmt] = 'None'
train$BsmtExposure[idx_Bsmt] = 'None'
train$BsmtFinType1[idx_Bsmt] = 'None'
train$BsmtFinType2[idx_Bsmt] = 'None'

train$MasVnrType[is.na(train$MasVnrType)] = 'None'
train$MasVnrArea[is.na(train$MasVnrArea)] = 0


# so now, I will look at the last two missing values...
idx_BsmtExposure = which(is.na(train$BsmtExposure))
idx_BsmtFinType2 = which(is.na(train$BsmtFinType2))



# it would appear that BsmntFinType2 missing value is Unf.  To show this, I will
# look at all the results from which TotalBsmtSF != BsmtUnfSF

# i have found another issue.  There are incorrect values for some of the basement entries.



# View(train %>% 
#   filter( (BsmtFinSF1 + BsmtFinSF2) == TotalBsmtSF)) %>% 
#   select(., BsmtFinType1, BsmtFinSF1, BsmtFinType2, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF)


# if BsmtFinSF1 + BsmtFinSF2 == TotalBsmtSF and BsmtFinSF2 == 0, then BsmtFinType2 = None

# if BsmtFinSF1 == TotalBsmtSF, then BsmtFinType2 = None
# if BsmtFinSF2 == TotalBsmtSF, then BsmtFinType1 = None

i=0
for (i in c(1:length(train$BsmtFinSF1))){
  if (train$BsmtFinSF1[i] == train$TotalBsmtSF[i]){
    train$BsmtFinType2[i] = 'None'
  }
  if (train$BsmtFinSF2[i] == train$TotalBsmtSF[i]){
    train$BsmtFinType1[i] = 'None'
  }
}

# impute BsmtFinType2 missing data point to be Unf
train$BsmtFinType2[is.na(train$BsmtFinType2)] = 'Unf'

# impute BsmtExpsure

# # check relation with TotalBsmtSF = 936
# library(ggplot2)
# ggplot(data=train, aes(x=TotalBsmtSF, fill=BsmtExposure)) +
#   geom_histogram() +
#   xlim(500,1000)
# # this plot indicates the value of Av to be the most frequently used for that particular square footage.

train$BsmtExposure[is.na(train$BsmtExposure)] = 'Av'

train[c(1,21:40)]
write.csv(train[c(1,21:40)], 'train21_40.csv')
