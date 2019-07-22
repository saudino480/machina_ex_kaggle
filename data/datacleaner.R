library(dplyr)

add_level <- function(col, val='None'){
  #Adds a level to the column factor, replaces all NA"s with val
  levels(col)[length(levels(col))+1] <- val
  col[is.na(col)] <- val
  return (col)
}

ready_df <- function(train_df, test_df){
  #Cleans/Imputes Kaggle Housing Dataset
  
  df <- rbind(train_df %>% select(-SalePrice), test_df)
  
  
  ######Imputation######
  
  # LotFrontage is highly correlated with LotArea. So I imputed on Simple Linear Regression
  model = lm(LotFrontage ~ LotArea,
             data = df %>% filter(!is.na(LotFrontage) &  (LotArea < 150000) & (LotFrontage < 300)))
  df$LotFrontage[is.na(df$LotFrontage)] = predict(model, df[is.na(df$LotFrontage),])
  
  
  # MCAR: Missing Values match up with BldgType='1Fam' and HouseStyle='1Story. 
  df$MSZoning[is.na(df$MSZoning)] <- "RL"
  
  # Imputed based on Mode on those features.
  df$BsmtUnfSF[is.na(df$BsmtCond)] <- 0
  df$TotalBsmtSF[is.na(df$BsmtCond)] <- 0
  df$BsmtHalfBath[is.na(df$BsmtCond)] <- 0
  
  
  df$MasVnrType[(is.na(df$MasVnrType) & !is.na(df$MasVnrArea))] <- 'None'
  df$MasVnrType[is.na(df$MasVnrType)] = "BrkFace"
  # Imputed on the Mean value of Areas for Type == 'BrkFace'
  df$MasVnrArea[is.na(df$MasVnrArea)] = 261.6724
  
  # MAR: Imputed based on Mode.
  df$Functional[is.na(df$Functional)] <- 'Typ' 
  
  # No Variance in column, imputed on Mode
  df$Utilities[is.na(df$Utilities)] <- 'AllPub' 
  
  # Imputed on Mode/Mean of values where GarageType is Detched
  df$GarageCars[is.na(df$GarageCars)] <- 1
  df$GarageArea[is.na(df$GarageArea)] <- 419.4923
  
  # After YearBuilt = 1970, all Electrical all of type SBrkr
  df$Electrical[is.na(df$Electrical)] <- 'SBrkr'
  
  # MCAR Imputed on Mode
  df$KitchenQual[is.na(df$KitchenQual)] <- 'TA'
  
  # MCAR Imputed on Mode.
  df$SaleType[is.na(df$SaleType)] <- 'WD'
  
  # MAR Imputed on Mode from RoofMatl
  df$Exterior1st[is.na(df$Exterior1st) & df$RoofMatl =='Tar&Grv'] <- 'CBlock'
  df$Exterior2nd[is.na(df$Exterior2nd) & df$RoofMatl =='Tar&Grv'] <- 'CBlock'
  
  
  
  
  #######Replacement######
  # Replaces NA values with values listed in data_description.txt where specified
  df$BsmtCond <- add_level(df$BsmtCond)
  df$BsmtQual <- add_level(df$BsmtQual)
  df$BsmtExposure <- add_level(df$BsmtExposure)
  df$BsmtFinType2 <- add_level(df$BsmtFinType2)
  df$BsmtFinType1 <- add_level(df$BsmtFinType1)
  # MAR Replaced with 0, Based on other BSMT Values
  df$BsmtFinSF1[df$BsmtQual == 'None' & is.na(df$BsmtFinSF1)] <- 0
  df$BsmtFinSF2[df$BsmtQual == 'None' & is.na(df$BsmtFinSF2)] <- 0
  df$BsmtFullBath[df$BsmtQual == 'None' & is.na(df$BsmtFullBath)] <- 0
  
  df$Alley <- add_level(df$Alley)
  df$MiscFeature <- add_level(df$MiscFeature)
  df$Fence <- add_level(df$Fence)
  df$PoolQC <- add_level(df$PoolQC)
  df$FireplaceQu <- add_level(df$FireplaceQu)
  df$GarageCond <- add_level(df$GarageCond)
  df$GarageFinish <- add_level(df$GarageFinish)
  df$GarageType <- add_level(df$GarageType)
  df$GarageQual <- add_level(df$GarageQual)
  df$GarageYrBlt <- add_level(df$GarageYrBlt, val = 9999)
  
  df$BsmtUnfSF[is.na(df$BsmtCond)] <- 0
  df$TotalBsmtSF[is.na(df$BsmtCond)] <- 0
  df$BsmtHalfBath[is.na(df$BsmtCond)] <- 0
  
  ######Factorize Character Columns######
  df$MSSubClass <- as.factor(df$MSSubClass)
  df$OverallCond <- as.factor(df$OverallCond)
  df$OverallQual <- as.factor(df$OverallQual)
  
  
  ######Package for Return######
  SalePrice <- train_df$SalePrice
  sales <- data.frame("SalePrice" = train_df$SalePrice)

  
  list(train_ready = cbind(df[1:1460,],sales),
       test_ready = df[1461:2919,])
}

## Categorical Binning
cat_binning <- function(train_df, test_df){
  df <- rbind(train_df %>% select(-SalePrice),test_df)
  
  
  
  SalePrice <- train_df$SalePrice
  sales <- data.frame("SalePrice" = train_df$SalePrice)
  
  list(train_binned = cbind(df[1:1460,],sales),
       test_binned = df[1461:2919,])
}




test <- read.csv("data/Raw Data/test.csv")
train <- read.csv('data/Raw Data/train.csv')
list2env(ready_df(train, test),env=environment())


list2env(cat_binning(train_ready,test_ready),env=environment())



write.csv(x=train_binned,file = 'Charlie/datafiles/clean_train.csv',row.names = F)
write.csv(x=test_binned,file = 'Charlie/datafiles/clean_test.csv',row.names = F)
