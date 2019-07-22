library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)
library(fastDummies)
library(lubridate)

nested_ifelse = function(dfcol, list, i = 0) {
    ifelse((dfcol == list[i+1]), i, nested_ifelse(dfcol, list, i+1))
}


finalPass = function(df) {

  final_pass = df  
  
  final_pass$dtsold = as.character(final_pass$dtsold)
  
  final_pass$dtsold[is.na(final_pass$dtsold)] = "0000-00"
  
  final_pass$yrsold = as.numeric(substr(final_pass$dtsold, 0, 4))
  
  final_pass$monthsold = as.numeric(substr(final_pass$dtsold, 6, 7))
  
  final_pass$dtsold = NULL
  
  final_pass$roofstyle[is.na(final_pass$roofstyle)] = "None"
  
  roofstyle_list = unique(final_pass$roofstyle)
  final_pass$roofstyle = nested_ifelse(final_pass$roofstyle, roofstyle_list)
  
  
  #DUMMY ALL THE THINGS
  idx_list = c(89:92, 94:100, 102, 106, 115)
  temp = colnames(final_pass)[idx_list]
  
  for (col in temp) {
    final_pass[[col]] = as.character(final_pass[[col]])
    if (length(final_pass[[col]][is.na(final_pass[[col]])] > 0)) {
      final_pass[[col]][is.na(final_pass[[col]])] = "None"
    }
    col_list = unique(final_pass[[col]])
    print(col_list)
    final_pass[[col]] = nested_ifelse(final_pass[[col]], col_list)
  }
  
  return(final_pass)
  
}


SamsCleaner = function(df) {
  
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
  my_slice$Utilities = as.character(my_slice$Utilities)
  if (length(my_slice$Utilities[is.na(my_slice$Utilities)] > 0)) {
    my_slice$Utilities[is.na(my_slice$Utilities)] = "None"
  }
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
  my_slice$LotShape = as.character(my_slice$LotShape)
  if (length(my_slice$LotShape[is.na(my_slice$LotShape)] > 0)) {
    my_slice$LotShape[is.na(my_slice$LotShape)] = "None"
  }
  lot_list = unique(my_slice$LotShape)
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
  my_slice$MSZoning = as.character(my_slice$MSZoning)
  if (length(my_slice$MSZoning[is.na(my_slice$MSZoning)] > 0)) {
    my_slice$MSZoning[is.na(my_slice$MSZoning)] = "None"
  }
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
  if (length(df$BsmtCond[is.na(df$BsmtCond)] > 0)) {
    df$BsmtCond[is.na(df$BsmtCond)] = 'None'
  }
  
  df$BsmtExposure[idx_Bsmt] = 'None'
  df$BsmtFinType1[idx_Bsmt] = 'None'
  df$BsmtFinType2[idx_Bsmt] = 'None'
  
  df$MasVnrType[is.na(df$MasVnrType)] = 'None'
  df$MasVnrArea[is.na(df$MasVnrArea)] = 0
  df$Exterior1st = as.character(df$Exterior1st)
  if (length(df$Exterior1st[is.na(df$Exterior1st)] > 0)) {
    df$Exterior1st[is.na(df$Exterior1st)] = "VinylSd"
  }
  
  df$Exterior2nd = as.character(df$Exterior2nd)
  if (length(df$Exterior2nd[is.na(df$Exterior2nd)] > 0)) {
    df$Exterior2nd[is.na(df$Exterior2nd)] = "None"
  }
  
  # so now, I will look at the last two missing values...
  idx_BsmtExposure = which(is.na(df$BsmtExposure))
  idx_BsmtFinType2 = which(is.na(df$BsmtFinType2))
  
  
  
  # it would appear that BsmntFinType2 missing value is Unf.  To show this, I will
  # look at all the results from which TotalBsmtSF != BsmtUnfSF
  
  # i have found another issue.  There are incorrect values for some of the basement entries.
  
  if (length(df$BsmtUnfSF[is.na(df$BsmtUnfSF)] > 0)) {
    df$BsmtUnfSF[is.na(df$BsmtUnfSF)] = 0
  }
  
  
  # View(df %>% 
  #   filter( (BsmtFinSF1 + BsmtFinSF2) == TotalBsmtSF)) %>% 
  #   select(., BsmtFinType1, BsmtFinSF1, BsmtFinType2, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF)
  
  
  # if BsmtFinSF1 + BsmtFinSF2 == TotalBsmtSF and BsmtFinSF2 == 0, then BsmtFinType2 = None
  
  # if BsmtFinSF1 == TotalBsmtSF, then BsmtFinType2 = None
  # if BsmtFinSF2 == TotalBsmtSF, then BsmtFinType1 = None
  df$BsmtFinSF1 = as.numeric(df$BsmtFinSF1)
  df$BsmtFinSF = as.numeric(df$BsmtFinSF1)
  
  if (length(df$BsmtFinSF1[is.na(df$BsmtFinSF1)] > 0)) {
    df$BsmtFinSF1[is.na(df$BsmtFinSF1)] = 0
  }
  
  if (length(df$BsmtFinSF2[is.na(df$BsmtFinSF2)] > 0)) {
    df$BsmtFinSF2[is.na(df$BsmtFinSF2)] = 0
  }
  
  if (length(df$TotalBsmtSF[is.na(df$TotalBsmtSF)] > 0)) {
    df$TotalBsmtSF[is.na(df$TotalBsmtSF)] = 0
  }
  
  i=0
  for (i in c(1:length(df$BsmtFinSF1))){
    print(paste("SF1 = ", df$BsmtFinSF1[i], "@", i))
    print(paste("TSF = ", df$TotalBsmtSF[i]))
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

CharlieClean = function(df) {
    df = dplyr::select(df,Id, 61:79)
    
    df$DtSold = paste(df$YrSold, sprintf('%02d',df$MoSold),'01',sep='-') %>% lubridate::ymd()
    df = dplyr::select(df,-YrSold,-MoSold)
    
    df[order(df$GarageArea),] %>% select('Id','GarageArea','GarageQual','GarageCond','GarageCars')
    #mice::md.pattern(df %>% select(c('GarageFinish','GarageQual','GarageArea','GarageCond')))
    if (length(df$GarageArea[is.na(df$GarageArea)] > 0)) {
      df$GarageArea[is.na(df$GarageArea)] = 0
    }
    
    df$GarageCond[df$GarageArea == 0] = 'None'
    if (length(df$GarageCond[is.na(df$GarageCond)] > 0)) {
      df$GarageCond[is.na(df$GarageCond)] = 'None'
    }
    print(unique(df$GarageCond))
    df$GarageFinish[df$GarageArea == 0] = 'None'
    if (length(df$GarageFinish[is.na(df$GarageFinish)] > 0)) {
      df$GarageFinish[is.na(df$GarageFinish)] = 'None'
    }
    print(unique(df$GarageFinish))
    df$GarageQual[df$GarageArea == 0] = 'None'
    if (length(df$GarageQual[is.na(df$GarageQual)] > 0)) {
      df$GarageQual[is.na(df$GarageQual)] = 'None'
    }
    print(unique(df$GarageQual))
    df$GarageCars[df$GarageArea == 0] = 0
    df$PoolQC[df$PoolArea == 0] = 'None'
    if (length(df$PoolQC[is.na(df$PoolQC)] > 0)) {
      df$PoolQC[is.na(df$PoolQC)] = 'None'
    }
    
    df$Fence[is.na(df$Fence)] = 'None'
    df$MiscFeature[df$MiscVal == 0] = 'None'
    if (length(df$MiscFeature[is.na(df$MiscFeature)] > 0)) {
      df$MiscFeature[is.na(df$MiscFeature)] = 'None'
    }
    #mice::md.pattern(df %>% select(c('MiscFeature','MiscVal','PoolQC','Fence')))
    if (length(df$SaleType[is.na(df$SaleType)] > 0)) {
      df$SaleType[is.na(df$SaleType)] = 0
    }
    if (length(df$GarageFinish[is.na(df$GarageFinish)]) > 0) {
      df$GarageFinish[is.na]
    }
    
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
    
    if (length(df$SaleType.fcon[is.na(df$SaleType.fcon)] > 0)) {
      df$SaleType.fcon[is.na(df$SaleType.fcon)] = 0
    }
    
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
  temp = temp %>%
    select(-dummy_cols)
  return(temp)
}


full_clean = function(read_path, write_path, dummies = FALSE) {
    housing = read.csv(read_path, header=TRUE,sep=",",stringsAsFactors = FALSE)
  
    cleanS = SamsCleaner(housing)
    cleanMD = MikeDClean(housing)
    cleanC = CharlieClean(housing)
    cleanMJ = MikeJrClean(housing)
    
    colnames(cleanS) = tolower(colnames(cleanS))
    colnames(cleanMD) = tolower(colnames(cleanMD))
    colnames(cleanC) = tolower(colnames(cleanC))
    colnames(cleanMJ) = tolower(colnames(cleanMJ))
    
    df_list = list(cleanS, cleanMD, cleanMJ, cleanC)
    
    full_df = df_list[[1]]
    for (i in 2:length(df_list)) {
      full_df = merge(full_df, df_list[[i]], by = 'id')
    }
    
    print(colnames(full_df[,('id' %in% colnames(full_df))]))
    
    if (dummies) {
      full_df = finalPass(full_df)
    }
    
    
    
    write.csv(full_df, write_path)
    
    return(full_df)
}
