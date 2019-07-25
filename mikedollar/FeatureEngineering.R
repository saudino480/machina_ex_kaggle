train <- read.csv('../data/clean_train.csv', stringsAsFactors = F)
test <- read.csv('../data/clean_test.csv', stringsAsFactors = F)


# Engineer Following features:
# TotalSpaceSF = TotalBsmtSF + X1stFlrSF + X2ndFlrSF
# LotFA = np.sqrt(LotArea) + LotFrontage

# TotalBath = FullBath + HalfBath
# RmsPerLivSpc = TotRmsAbvGrd/(X1stFlrSF + X2ndFlrSF)
# FPperSF = FirePlaces/TotalSpaceSF

fe_drop =c('X3SsnPorch','GrLivArea', 'Fireplaces', 'HalfBath', 
           'BsmtHalfBath','BsmtFullBath', 'LowQualFinSF', 
           'BsmtUnfSF','EnclosedPorch','KitchenAbvGr',
           'BedroomAbvGr','FullBath', 'HalfBath',
           'BsmtFinType2','TotalBsmtSF', 'X1stFlrSF', 
           'X2ndFlrSF', 'TotRmsAbvGrd', 'LotArea', 
           'LotFrontage', 'PoolArea', 'MoSold', 'YrSold')

# Train
train = train %>% 
  mutate(., 
         TotalSpaceSF = (TotalBsmtSF + X1stFlrSF + X2ndFlrSF), 
         LotFA = (LotArea)^.5 + LotFrontage, 
         RmsPerLivSpc = TotRmsAbvGrd/(X1stFlrSF + X2ndFlrSF),
         TotalBath = FullBath + HalfBath,
         RmsPerLivSpc = TotRmsAbvGrd/(X1stFlrSF + X2ndFlrSF),
         FPperSF = Fireplaces/TotalSpaceSF
         )
train = train %>% 
  select(., -fe_drop)

# Test
test = test %>% 
  mutate(., 
         TotalSpaceSF = (TotalBsmtSF + X1stFlrSF + X2ndFlrSF), 
         LotFA = (LotArea)^.5 + LotFrontage, 
         RmsPerLivSpc = TotRmsAbvGrd/(X1stFlrSF + X2ndFlrSF),
         TotalBath = FullBath + HalfBath,
         RmsPerLivSpc = TotRmsAbvGrd/(X1stFlrSF + X2ndFlrSF),
         FPperSF = Fireplaces/TotalSpaceSF
         )
test = test %>% 
  select(., -fe_drop)


# add toggle for toggle_list
need_toggle = c('YearRemodAdd', 'MasVnrArea',
                'BsmtFinSF1', 'BsmtFinSF2',
                'GarageArea', 'WoodDeckSF',
                'OpenPorchSF', 'ScreenPorch')
train = train %>% 
  mutate(., YearRemodAdd_tog = ifelse(YearRemodAdd==min(YearRemodAdd), 0,1),
            MasVnrArea_tog = ifelse(MasVnrArea==min(MasVnrArea), 0,1),
            BsmtFinSF1_tog = ifelse(BsmtFinSF1==min(BsmtFinSF1), 0,1),
            BsmtFinSF2_tog = ifelse(BsmtFinSF2==min(BsmtFinSF2), 0,1),
            GarageArea_tog = ifelse(GarageArea==min(GarageArea), 0,1),
            WoodDeckSF_tog = ifelse(WoodDeckSF==min(WoodDeckSF), 0,1),
            OpenPorchSF_tog = ifelse(OpenPorchSF==min(OpenPorchSF), 0,1),
            ScreenPorch_tog = ifelse(ScreenPorch==min(ScreenPorch), 0,1))

  
test = test %>% 
  mutate(., YearRemodAdd_tog = ifelse(YearRemodAdd==min(YearRemodAdd), 0,1),
  MasVnrArea_tog = ifelse(MasVnrArea==min(MasVnrArea), 0,1),
  BsmtFinSF1_tog = ifelse(BsmtFinSF1==min(BsmtFinSF1), 0,1),
  BsmtFinSF2_tog = ifelse(BsmtFinSF2==min(BsmtFinSF2), 0,1),
  GarageArea_tog = ifelse(GarageArea==min(GarageArea), 0,1),
  WoodDeckSF_tog = ifelse(WoodDeckSF==min(WoodDeckSF), 0,1),
  OpenPorchSF_tog = ifelse(OpenPorchSF==min(OpenPorchSF), 0,1),
  ScreenPorch_tog = ifelse(ScreenPorch==min(ScreenPorch), 0,1))                       
          

library(fastDummies)

write.csv(train, 'clean_data/train_fe_dum.csv', row.names = F)
write.csv(test, 'clean_data/test_fe_dum.csv', row.names = F)
