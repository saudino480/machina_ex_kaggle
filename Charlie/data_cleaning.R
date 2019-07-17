library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)
library(mice)


df <- read.table('data/train.csv',header=TRUE,sep=",")

df = dplyr::select(df,Id, 61:79)

df$DtSold = paste(df$YrSold, sprintf('%02d',df$MoSold),'01',sep='-') %>% ymd()
df = dplyr::select(df,-YrSold,-MoSold)

df[order(df$GarageArea),] %>% select('Id','GarageArea','GarageQual','GarageCond','GarageCars')

df %>% mutate(GarageQual = ifelse(GarageArea == 0, 'None',))
