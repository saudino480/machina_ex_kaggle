library(dplyr)
library(forcats)

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

cat_stat_sheet <- function(train, test) {
  for (i in names(train)) {
    if (is.factor(train[,i])) {
      print(paste('*****',i,'*****'))
      SalesStats = train %>% group_by_(i) %>% 
        dplyr::summarise(t = median(SalePrice),m = mean(SalePrice),sd = sd(SalePrice))
      names(SalesStats) = c('Var1','t','m','sd')
      out = merge(data.frame(table(train[,i])),data.frame(table(test[,i])),by='Var1',all = T)
      out <- merge(out,SalesStats,by='Var1',all=T)
      names(out) <- c('Name','Train','Test','SalesMed','SalesMean','SalesSD')
      print(out %>% arrange(SalesMed))
    }
  }
  return (NA)
}



create_other_train <- function(col, perc=0.05) {
  # Create an 'othr' factor for levels with a population of less than perc (default 0.05)
  pop = list()
  for (i in levels(col)) {
    if (count(col == i)[2,2] < nrow(train) * perc) {
      pop <- c(pop, i)
    }
  }
  return(col <- col %>% fct_collapse(Othr = pop))
}

create_other_test <- function(col, trained){
  trained_factors <- levels(trained)
  test_factors <- levels(col)
  col <- col %>% fct_collapse(Othr = test_factors[test_factors %ni% trained_factors])
  return (col)
}
  
create_other_total <- function(col, perc=0.05) {
  # Create an 'othr' factor for levels with a population of less than perc (default 0.05)
  pop = list()
  for (i in levels(col)) {
    if (count(col == i)[2,2] < 2919 * perc) {
      pop <- c(pop, i)
    }
  }
  return(col <- col %>% fct_collapse(Othr = pop))
}


## Categorical Binning
cat_binning <- function(train_df, test_df){
  df <- rbind(train_df %>% select(-SalePrice),test_df)
  
  ###### Write Functions Within Here ######
  ## To Be Binned ##

  df$LotShape <- df$LotShape %>% fct_collapse(IRG = c('IR1','IR2','IR3'))
  df$MasVnrType <- df$MasVnrType %>% fct_collapse(None = c('BrkCmn','None'))
  
  #cat_stat_sheet(df, train_df, test_df)
  #df$MSSubClass = df$MSSubClass %>% fct_collapse(Othr = c('50', 'Othr')) %>%
                                    #fct_collapse(High = c('120', '60'))
  
  df$Alley = df$Alley %>% fct_collapse(None = c('None', 'Pave'))
  
  df$MSZoning = df$MSZoning %>% fct_collapse(Othr = c('RH', 'RM'))
  
  df$LotConfig = df$LotConfig %>% fct_collapse(NotCulD = c('Corner', 'Inside', 'FR2')) %>%
                                  fct_collapse(CulD = c('CulDSac', 'FR3'))
  
  df$LandSlope = df$LandSlope %>% fct_collapse(Othr = c('Mod', 'Sev'))
  
  df$Neighborhood = df$Neighborhood %>% fct_collapse(Lowest = c('MeadowV', 'IDOTRR', 'BrDale')) %>%
                                        fct_collapse(Low = c('OldTown', 'Edwards', 'BrkSide')) %>%
                                        fct_collapse(Med_Low  = c('Sawyer', 'Blueste', 'SWISU', 'NAmes')) %>%
                                        fct_collapse(Med = c('NPkVill', 'Mitchel')) %>%
                                        fct_collapse(True_Med = c('SawyerW', 'Gilbert', 'NWAmes')) %>%
                                        fct_collapse(Med_High = c('Blmngtn', 'CollgCr', 'ClearCr', 'Crawfor')) %>%
                                        fct_collapse(High = c('Veenker', 'Somerst', 'Timber')) %>%
                                        fct_collapse(Lux = c('StoneBr', 'NoRidge', 'NridgHt'))
  
  df$Condition1 = df$Condition1 %>% fct_collapse(High = c('RRNn', 'PosA', 'PosN', 'RRNe')) %>%
                                    fct_collapse(Med = c('RRAn', 'Norm', 'RRAn')) %>%
                                    fct_collapse(Low = c('Feedr', 'Artery'))
  
  df$OverallCond = df$OverallCond %>% fct_collapse(Low = c('1', '2', '3'))
  
  
  df$BldgType = df$BldgType %>% fct_collapse(fmCon = c('2fmCon', 'Duplex', 'Twnhs')) %>%
                                fct_collapse(Fam = c('1Fam', 'TwnhsE'))
  
  df$HouseStyle = df$HouseStyle %>% fct_collapse(Low = c('1.5Fin', '2.5Unf', 'SFoyer')) %>%
                                    fct_collapse(Med = c('1Story', 'SLvl')) %>%
                                    fct_collapse(High = c('2Story', '2.5Fin'))
  
  df$RoofStyle = df$RoofStyle %>% fct_collapse(Low = c('Gambrel', 'Gable')) %>%
                                  fct_collapse(Med = c('Mansard', 'Hip', 'Flat')) %>%
                                  fct_collapse(High = c('Shed'))
  
  df$RoofMatl = df$RoofMatl %>% fct_collapse(Low = c('Roll', 'ClyTile', 'CompShg', 'Tar&Grv', 'Metal')) %>%
                                fct_collapse(Med = c('Membran', 'WdShake')) %>%
                                fct_collapse(High = c('WdShngl'))
  
  df$Exterior1st = df$Exterior1st %>% fct_collapse(Low = c('BrkComm','AsphShn', 'CBlock', 'AsbShng')) %>%
                                      fct_collapse(Med = c('WdShing', 'Wd Sdng', 'MetalSd')) %>%
                                      fct_collapse(High = c('Stucco', 'HdBoard')) %>%
                                      fct_collapse(Highest = c('BrkFace', 'Plywood')) %>%
                                      fct_collapse(Lux = c('Stone', 'ImStucc'))
    
  df$Exterior2nd = df$Exterior2nd %>% fct_collapse(Lowest = c('CBlock', 'AsbShng')) %>%
                                      fct_collapse(Low = c('Wd Sdng', 'Wd Shng', 'MetalSd', 'AsphShn',
                                                           'Stucco', 'Brk Cmn')) %>%
                                      fct_collapse(Med = c('HdBoard', 'BrkFace', 'Plywood')) %>%
                                      fct_collapse(High = c('Stone', 'ImStucc')) %>%
                                      fct_collapse(Highest = c('CmentBd', 'Other'))
  
                        
  df$ExterCond = df$ExterCond %>% fct_collapse(TA = c('TA', 'Ex')) %>%
                                  fct_collapse(Po = c('Po', 'Fa'))
  
  df$Foundation = df$Foundation %>% fct_collapse(Stone = c('CBlock', 'Stone')) %>%
                                    fct_collapse(Other = c('Wood', 'PConc'))
  
  df$BsmtQual = df$BsmtQual %>% fct_collapse(None = c('None', 'Fa'))
  
  df$BsmtCond = df$BsmtCond %>% fct_collapse(Med = c('None', 'Fa')) %>%
                                fct_collapse(High = c('TA', 'Gd'))
  
  df$BsmtExposure = df$BsmtExposure %>% fct_collapse(Med = c('Mn', 'Av'))
  
  df$BsmtFinType1 = df$BsmtFinType1 %>% fct_collapse(Low = c('LwQ', 'BLQ', 'Rec')) %>%
                                        fct_collapse(Med = c('ALQ', 'Unf')) %>%
                                        fct_collapse(High = c('GLQ'))
  
  df$HeatingQC = df$HeatingQC %>% fct_collapse(Med = c('Fa', 'TA', 'Gd'))
  
  df$Electrical = df$Electrical %>% fct_collapse(Low = c('Mix', 'FuseP')) %>%
                                    fct_collapse(Med = c('FuseF', 'FuseA')) %>%
                                    fct_collapse(High = c('SBrkr'))
  
  df$Functional = df$Functional %>% fct_collapse(Low = c('Maj2')) %>%
                                    fct_collapse(Med = c('Sev', 'Mod', 'Min1', 'Min2', 'Maj1')) %>%
                                    fct_collapse(High = c('Typ'))
  
  df$FireplaceQu = df$FireplaceQu %>% fct_collapse(Low = c('Po', 'None')) %>%
                                      fct_collapse(TA = c('TA', 'Gd'))
  
  df$GarageType = df$GarageType %>% fct_collapse(Low = c('None', 'CarPort')) %>%
                                    fct_collapse(Med = c('Detchd', 'Basment', '2Types')) %>%
                                    fct_collapse(High = c('Attchd')) %>%
                                    fct_collapse(Highest = c('BuiltIn'))
                                    
  df$PoolQC = df$PoolQC %>% fct_collapse(Low = c('None', 'Gd', 'Fa')) %>%
                            fct_collapse(High = c('Ex'))
  
  df$Fence = df$Fence %>% fct_collapse(Low = c('MnWw')) %>%
                          fct_collapse(Med = c('MnPrv', 'GdWo')) %>%
                          fct_collapse(High = c('GdPrv'))
  
  df$SaleType = df$SaleType %>% fct_collapse(Low = c('Oth', 'ConLI')) %>%
                                fct_collapse(Med = c('COD', 'ConLD', 'ConLw', 'WD')) %>%
                                fct_collapse(High = c('New', 'Con'))
                                
  df$SaleCondition = df$SaleCondition %>% fct_collapse(Low = c('AdjLand')) %>%
                                          fct_collapse(Med = c('Abnorml', 'Family', 'Alloca')) %>%
                                          fct_collapse(Med_High = c('Normal')) %>%
                                          fct_collapse(High = c('Partial'))
  
  
  
  
  
                                    
  
  
  
  #df$KitchenQual = df$KitchenQual %>% fct_collapse(TA = c('TA', 'Othr'))
  
  #df$GarageType = df$GarageType %>% fct_collapse(Detchd = c('Othr', Detchd))
  
  #df$SaleCondition = df$SaleCondition %>% fct_collapse(Othr = c('Othr', 'Abnorml'))
  
  
  
  ### Bin Categories by Frequency of Appearance. Default is 5%
  #for (i in names(df)) {
  #  if (is.factor(df[,i])) {
  #    df[,i] <- create_other_total(df[,i])
  #  }
  #}
  
  
  
  
  ###### STOP HERE ######
  
  SalePrice <- train_df$SalePrice
  sales <- data.frame("SalePrice" = train_df$SalePrice)
  
  list(train_binned = cbind(df[1:1460,],sales),
       test_binned = df[1461:2919,])
}


test <- read.csv("data/Raw Data/test.csv")
train <- read.csv('data/Raw Data/train.csv')
list2env(ready_df(train, test),env=environment())


list2env(cat_binning(train_ready,test_ready),env=environment())


cat_stat_sheet(train_binned, test_binned)

write.csv(x=train_ready,file = 'data/clean_train_beforebin.csv',row.names = F)
write.csv(x=test_ready,file = 'data/clean_test_beforebin.csv',row.names = F)


write.csv(x=train_binned,file = 'data/clean_train.csv',row.names = F)
write.csv(x=test_binned,file = 'data/clean_test.csv',row.names = F)


