library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)
library(fastDummies)
library(lubridate)

nested_ifelse = function(dfcol, list, i = 0) {
  ifelse(dfcol == list[i+1], i, nested_ifelse(dfcol, list, i+1))
}

SamsCleaner = function(df) {
  
  test = df %>%
    filter(Id == 949)
  
  my_slice = df[,1:21]
  
  #### LotArea ####
  # Removed LotFrontage, added area to Lot Area.
  my_slice$LotFrontage[is.na(my_slice$LotFrontage)] = 0
  
  my_slice$LotArea = my_slice$LotFrontage + my_slice$LotArea
  
  my_slice$LotFrontage = NULL
  
  #### Street ####
  #new street values are 0 for Gravel and 1 for Pavement
  my_slice$Street = ifelse(my_slice$Street == "Grvl", 0, 1)
  
  
  #### Alley ####
  #impute alley NA's.
  my_slice$Alley = as.character(my_slice$Alley)
  my_slice$Alley[is.na(my_slice$Alley)] = "None"
  
  #1 = T, 0 = F. If both are 0 it implies there is no alley.
  my_slice$Alley_Grvl = ifelse(my_slice$Alley == "Grvl", 1, 0)
  my_slice$Alley_Pave = ifelse(my_slice$Alley == "Pave", 1, 0)
  my_slice$Alley = NULL
  
  
  #### House Style ####
  my_slice$HouseStyle = NULL
  
  
  #### Utilities ####
  #Currently:
  #         0 = Not All Included
  #         1 = All Included
  #
  my_slice$Utilities = ifelse(my_slice$Utilities == "AllPub", 1, 0)
  
  #### Land Slope ####
  #both 0 if Gentle Slope, 1 otherwise.
  my_slice$LandSlope_Sev = ifelse(my_slice$LandSlope == "Sev", 1, 0)
  my_slice$LandSlope_Mod = ifelse(my_slice$LandSlope == "Mod", 1, 0)
  my_slice$LandSlope = NULL
  
  
  
  ##### Lot Shapes ####
  #Currently:
  #
  #         0 = Reg
  #         1 = IR1
  #         2 = IR2
  #         3 = IR3
  #
  lot_list = unique(as.character(my_slice$LotShape))
  my_slice$LotShape = nested_ifelse(my_slice$LotShape, lot_list)
  
  
  #### Land Contour ####
  #Currently:
  #
  #         0 = Level
  #         1 = Bank
  #         2 = Low
  #         3 = HLS
  #
  contour_list = unique(as.character(my_slice$LandContour))
  
  my_slice$LandContour = nested_ifelse(my_slice$LandContour, contour_list)
  
  #### Lot Config ####
  #elim FR3 category and cast rest to dummies, perhaps consider making this
  #1 = CulDSac, 0 = everything else
  #Currently:
  #
  #         0 = "Inside Lot"
  #         1 = "Frontage on at least 2 Sides"
  #         2 = "Corner Lot"
  #         3 = "CulDSac"
  #
  
  my_slice$LotConfig = as.character(my_slice$LotConfig)
  my_slice$LotConfig[my_slice$LotConfig == "FR3"] = "FR2"
  
  config_list = unique(my_slice$LotConfig)
  my_slice$LotConfig = nested_ifelse(my_slice$LotConfig, config_list)
  
  #### BldgType ####
  #consider eliminating the Twnhs or TwnhsE category, essentially similar
  #otherwise, encoding is:
  #
  #         0 = 1 Family
  #         1 = 2 Family Conversion
  #         2 = Duplex
  #         3 = Townhouse End Unit
  #         4 = Townhouse Inside Unit
  #         
  bldg_list = unique(as.character(my_slice$BldgType))
  my_slice$BldgType = nested_ifelse(my_slice$BldgType, bldg_list)
  
  
  #### MS Zoning ####
  #Currently:
  #
  #         0 = Commercial
  #         1 = Residential (Low Density)
  #         2 = Residential (Medium Density)
  #         3 = Residential (High Density)
  #         4 = Floating Village Residential
  #
  zoning_list = unique(as.character(my_slice$MSZoning))
  my_slice$MSZoning = nested_ifelse(my_slice$MSZoning, zoning_list)
  
  
  #### Conditions ####
  # -maybe consider turning this into a tuple?
  # This block handles BOTH Condition1 and Condition2
  # We paired down the nuance of the chart in the following way:
  #
  # NearStreet <- Artery, Feedr
  # NearTrain <- RRNn, RRNe, RRAn, RRAe
  # Pos_Att <- PosN, PosA
  # Currently:
  #
  #         0 = Normal
  #         1 = NearStreet
  #         2 = NearTrain
  #         3 = Residential (Low Density)
  #
  my_slice$Condition1 = as.character(my_slice$Condition1)
  my_slice$Condition2 = as.character(my_slice$Condition2)
  
  #consolidate
  nearTrain = c("RRNn", "RRNe", "RRAn", "RRAe")
  nearStreet = c("Artery", "Feedr")
  pos_att = c("PosN", "PosA")
  my_slice$Condition1[my_slice$Condition1 %in% nearTrain] = "nearTrain"
  my_slice$Condition1[my_slice$Condition1 %in% nearStreet] = "nearStreet"
  my_slice$Condition1[my_slice$Condition1 %in% pos_att] = "pos"
  my_slice$Condition2[my_slice$Condition2 %in% nearTrain] = "nearTrain"
  my_slice$Condition2[my_slice$Condition2 %in% nearStreet] = "nearStreet"
  my_slice$Condition2[my_slice$Condition2 %in% pos_att] = "pos"
  
  #dummy
  cond_list = unique(my_slice$Condition1)
  my_slice$Condition1 = nested_ifelse(my_slice$Condition1, cond_list)
  
  my_slice$Condition2 = nested_ifelse(my_slice$Condition2, cond_list)
  
  #making all lowercase, save to CSV
  colnames(my_slice) = tolower(colnames(my_slice))
  
  return(my_slice)
}

MikeDClean = function(df) {
  
  
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
  
  
  
  # View(df %>% 
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
  # ggplot(data=df, aes(x=TotalBsmtSF, fill=BsmtExposure)) +
  #   geom_histogram() +
  #   xlim(500,1000)
  # # this plot indicates the value of Av to be the most frequently used for that particular square footage.
  
  df$BsmtExposure[is.na(df$BsmtExposure)] = 'Av'
  
  return(df[c(1,21:40)])
 
}

CharlieClean = function(df, test = FALSE) {
    df = dplyr::select(df,Id, 61:79)
    
    df$DtSold = paste(df$YrSold, sprintf('%02d',df$MoSold),'01',sep='-') %>% lubridate::ymd()
    df = dplyr::select(df,-YrSold,-MoSold)
    
    df[order(df$GarageArea),] %>% select('Id','GarageArea','GarageQual','GarageCond','GarageCars')
    #mice::md.pattern(df %>% select(c('GarageFinish','GarageQual','GarageArea','GarageCond')))
    
    df$GarageCond[df$GarageArea == 0] = 'None'
    df$GarageFinish[df$GarageArea == 0] = 'None'
    df$GarageQual[df$GarageArea == 0] = 'None'
    df$PoolQC[df$PoolArea == 0] = 'None'
    df$Fence[is.na(df$Fence)] = 'None'
    df$MiscFeature[df$MiscVal == 0] = 'None'
    #mice::md.pattern(df %>% select(c('MiscFeature','MiscVal','PoolQC','Fence')))
    
    options(na.action='na.pass')
    
    PavedDrive.f = factor(df$PavedDrive)
    df = cbind(df %>% select(-PavedDrive),model.matrix(~PavedDrive.f)[,-1])
    SaleType.f = factor(df$SaleType)
    df = cbind(df %>% select(-SaleType),model.matrix(~SaleType.f)[,-1])
    GarageFinish.f = as.factor(df$GarageFinish)
    df = cbind(df %>% select(-GarageFinish),model.matrix(~GarageFinish.f)[,-1])
    GarageCond.f = factor(df$GarageCond)
    df = cbind(df %>% select(-GarageCond),model.matrix(~GarageCond.f)[,-1])
    GarageQual.f = factor(df$GarageQual)
    df = cbind(df %>% select(-GarageQual),model.matrix(~GarageQual.f)[,-1])
    PoolQC.f = factor(df$PoolQC)
    df = cbind(df %>% select(-PoolQC),model.matrix(~PoolQC.f)[,-1])
    Fence.f = factor(df$Fence)
    df = cbind(df %>% select(-Fence),model.matrix(~Fence.f)[,-1])
    MiscFeature.f = factor(df$MiscFeature)
    df = cbind(df %>% select(-MiscFeature),model.matrix(~MiscFeature.f)[,-1])
    #df %>% head()
    return(df)
}

MikeJrClean = function(df) {
  
  df = df %>% select('Id', 40:60)
  
  df$BsmtFullBath[is.na(df$BsmtFullBath)] = 0
  df$BsmtHalfBath[is.na(df$BsmtHalfBath)] = 0
  df$KitchenQual[is.na(df$KitchenQual)] = 'TA'
  df$Functional[is.na(df$Functional)] = 'Typ'
  df$GarageYrBlt[is.na(df$GarageYrBlt)] = 0000
  
  dummy_cols = c('HeatingQC','CentralAir','Electrical','GarageType','KitchenQual',
                 'Functional','FireplaceQu')
  
  
  temp = fastDummies::dummy_cols(df, select_columns = dummy_cols, remove_first_dummy = TRUE)
  
  return(temp)
}
