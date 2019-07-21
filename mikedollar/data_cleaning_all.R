library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)
library(mice)

#This cleaning file assumes the working directory of machina_ex_kaggle/mikedollar

train <- read.csv('../data/train.csv', stringsAsFactors = F)

# Sam #########################################################################
# 1 - 20

sam_slice = train[,1:20]
md.pattern(sam_slice, rotate.names = T)

# change LotFrontage NA's to 0
# change Alley NA's to 'None'
sam_slice$LotFrontage[is.na(sam_slice$LotFrontage)] = 0
sam_slice$Alley[is.na(sam_slice$Alley)] = 'None'

# change  MSSubClass to character type, because it is a numeric nominal categorical variable.
sam_slice$MSSubClass = as.character(sam_slice$MSSubClass)

md.pattern(sam_slice, rotate.names = T)

write.csv(sam_slice, 'clean_data/sam_slice_clean.csv', row.names = F)


# mikedollar #########################################################################
# 21 - 40

# just focusing on columns 21-40...

m = 21
n = 40

mike_slice = train[,c(1,m:n)]

md.pattern(mike_slice, rotate.names = T)


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

idx_Bsmt = which(is.na(mike_slice$BsmtQual))
mike_slice$BsmtQual[idx_Bsmt] = 'None'
mike_slice$BsmtCond[idx_Bsmt] = 'None'
mike_slice$BsmtExposure[idx_Bsmt] = 'None'
mike_slice$BsmtFinType1[idx_Bsmt] = 'None'
mike_slice$BsmtFinType2[idx_Bsmt] = 'None'

mike_slice$MasVnrType[is.na(mike_slice$MasVnrType)] = 'None'
mike_slice$MasVnrArea[is.na(mike_slice$MasVnrArea)] = 0


# so now, I will look at the last two missing values...
idx_BsmtExposure = which(is.na(mike_slice$BsmtExposure))
idx_BsmtFinType2 = which(is.na(mike_slice$BsmtFinType2))



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
for (i in c(1:length(mike_slice$BsmtFinSF1))){
  if (mike_slice$BsmtFinSF1[i] == mike_slice$TotalBsmtSF[i]){
    mike_slice$BsmtFinType2[i] = 'None'
  }
  if (mike_slice$BsmtFinSF2[i] == mike_slice$TotalBsmtSF[i]){
    mike_slice$BsmtFinType1[i] = 'None'
  }
}

# impute BsmtFinType2 missing data point to be Unf
mike_slice$BsmtFinType2[is.na(mike_slice$BsmtFinType2)] = 'Unf'

# impute BsmtExpsure

# # check relation with TotalBsmtSF = 936
# library(ggplot2)
# ggplot(data=train, aes(x=TotalBsmtSF, fill=BsmtExposure)) +
#   geom_histogram() +
#   xlim(500,1000)
# # this plot indicates the value of Av to be the most frequently used for that particular square footage.

mike_slice$BsmtExposure[is.na(mike_slice$BsmtExposure)] = 'Av'



# get list of categorical column names

# slice = df[colnames(df)[m:n]]
# 
# catlist = colnames(slice[, sapply(slice, class) == 'character'])


md.pattern(mike_slice, rotate.names = T)

write.csv(mike_slice, 'clean_data/mike_slice_clean.csv', row.names = F)


# mikejr ##########################################################################
mikejr_slice = train[,c(1,41:60)]

md.pattern(mikejr_slice, rotate.names = T)

mikejr_missing_colnames = c('Electrical', 'GarageType', 'GarageYrBlt', 'FireplaceQu')
View(mikejr_slice[mikejr_missing_colnames])

View(mikejr_slice[c('Fireplaces', 'FireplaceQu', 'Electrical')])
View(mikejr_slice)
# change Na's to 'None' for GarageType, GarageYrBlt, FireplaceQu
mikejr_slice$GarageType[is.na(mikejr_slice$GarageType)] = 'None'
mikejr_slice$GarageYrBlt[is.na(mikejr_slice$GarageYrBlt)] = 'None'
mikejr_slice$FireplaceQu[is.na(mikejr_slice$FireplaceQu)] = 'None'

# Looking at the histogram of YearBuilt, I can surmise that pretty much
# anything built after 1970 is probably sporting the class, SBrkr for 
# the column Electrical.  

# So impute the missing Electrical value with SBrkr.
mikejr_slice$Electrical[is.na(mikejr_slice$Electrical)] = 'SBrkR'

md.pattern(mikejr_slice, rotate.names = T)

write.csv(mikejr_slice, 'clean_data/mikejr_slice_clean.csv', row.names = F)

# Charlie #########################################################################
# 61 - 80

charlie_slice <- read.table('../data/train.csv',header=TRUE,sep=",",stringsAsFactors = FALSE)

charlie_slice = dplyr::select(charlie_slice,Id, 61:80)

charlie_slice$DtSold = paste(charlie_slice$YrSold, sprintf('%02d',charlie_slice$MoSold),'01',sep='-') %>% ymd()
charlie_slice = dplyr::select(charlie_slice,-YrSold,-MoSold)

charlie_slice[order(charlie_slice$GarageArea),] %>% select('Id','GarageArea','GarageQual','GarageCond','GarageCars')
mice::md.pattern(charlie_slice %>% select(c('GarageFinish','GarageQual','GarageArea','GarageCond')))

charlie_slice$GarageCond[charlie_slice$GarageArea == 0] = 'None'
charlie_slice$GarageFinish[charlie_slice$GarageArea == 0] = 'None'
charlie_slice$GarageQual[charlie_slice$GarageArea == 0] = 'None'
charlie_slice$PoolQC[charlie_slice$PoolArea == 0] = 'None'
charlie_slice$Fence[is.na(charlie_slice$Fence)] = 'None'
charlie_slice$MiscFeature[charlie_slice$MiscVal == 0] = 'None'
mice::md.pattern(charlie_slice %>% select(c('MiscFeature','MiscVal','PoolQC','Fence')))



md.pattern(charlie_slice, rotate.names = T)

write.csv(charlie_slice, 'clean_data/charlie_slice_clean.csv', row.names = F)

