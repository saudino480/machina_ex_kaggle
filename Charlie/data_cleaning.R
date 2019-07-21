library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)
library(mice)


df <- read.table('../data/train.csv',header=TRUE,sep=",",stringsAsFactors = FALSE)

df = dplyr::select(df,Id, 61:79)

df$DtSold = paste(df$YrSold, sprintf('%02d',df$MoSold),'01',sep='-') %>% ymd()
df = dplyr::select(df,-YrSold,-MoSold)

df[order(df$GarageArea),] %>% select('Id','GarageArea','GarageQual','GarageCond','GarageCars')
mice::md.pattern(df %>% select(c('GarageFinish','GarageQual','GarageArea','GarageCond')))

df$GarageCond[df$GarageArea == 0] = 'None'
df$GarageFinish[df$GarageArea == 0] = 'None'
df$GarageQual[df$GarageArea == 0] = 'None'
df$PoolQC[df$PoolArea == 0] = 'None'
df$Fence[is.na(df$Fence)] = 'None'
df$MiscFeature[df$MiscVal == 0] = 'None'
mice::md.pattern(df %>% select(c('MiscFeature','MiscVal','PoolQC','Fence')))

PavedDrive.f = factor(df$PavedDrive)
df = cbind(df %>% select(-PavedDrive),model.matrix(~PavedDrive.f)[,-1])
SaleType.f = factor(df$SaleType)
df = cbind(df %>% select(-SaleType),model.matrix(~SaleType.f)[,-1])
GarageFinish.f = factor(df$GarageFinish)
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
df %>% head()

write.table(df,file="Charlie/cleaned_data.csv")

