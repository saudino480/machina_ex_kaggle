# This script will clean the training set for columns 21 - 40.  For columns missing data,
# imputation was used based on the classifcation of MCAR, MAR, and MNAR.  There were only
# MCAR missing for this range.  The rest of the 'NA' values were used for classifications.
# Those values were replaced by 'None' for character entries.  Only one value was imputed 
# by discretion, and the rest were imputedbased on common sense.  For example, if a
# basement is non-existent, then it should not be described as unfinished.

# This script may need to be updated for the testing data set.


df <- read.csv('../data/train.csv', stringsAsFactors = F)

# specify column integer range:  m:n
m = 21
n = 40


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

idx_Bsmt = which(is.na(df$BsmtQual))
df$BsmtQual[idx_Bsmt] = 'None'
df$BsmtCond[idx_Bsmt] = 'None'
df$BsmtExposure[idx_Bsmt] = 'None'
df$BsmtFinType1[idx_Bsmt] = 'None'
df$BsmtFinType2[idx_Bsmt] = 'None'

df$MasVnrType[is.na(df$MasVnrType)] = 'None'
df$MasVnrArea[is.na(df$MasVnrArea)] = 0


# so now, I will look at the last two missing values...
idx_BsmtExposure = which(is.na(df$BsmtExposure))
idx_BsmtFinType2 = which(is.na(df$BsmtFinType2))



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
for (i in c(1:length(df$BsmtFinSF1))){
  if (df$BsmtFinSF1[i] == df$TotalBsmtSF[i]){
    df$BsmtFinType2[i] = 'None'
  }
  if (df$BsmtFinSF2[i] == df$TotalBsmtSF[i]){
    df$BsmtFinType1[i] = 'None'
  }
}

# impute BsmtFinType2 missing data point to be Unf
df$BsmtFinType2[is.na(df$BsmtFinType2)] = 'Unf'

# impute BsmtExpsure

# # check relation with TotalBsmtSF = 936
# library(ggplot2)
# ggplot(data=train, aes(x=TotalBsmtSF, fill=BsmtExposure)) +
#   geom_histogram() +
#   xlim(500,1000)
# # this plot indicates the value of Av to be the most frequently used for that particular square footage.

df$BsmtExposure[is.na(df$BsmtExposure)] = 'Av'

# change to lowercase
colnames(df)[m:n] = tolower(colnames(df)[m:n])

# get list of categorical column names

# slice = df[colnames(df)[m:n]]
# 
# catlist = colnames(slice[, sapply(slice, class) == 'character'])



write.csv(df[c(1,m:n)], 'train21_40.csv')


