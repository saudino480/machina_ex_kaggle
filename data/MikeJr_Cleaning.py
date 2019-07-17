### packages
import numpy as np
import pandas as pd


### load data
train_data = pd.read_csv('./data/test.csv')
### subset data
train_subset = pd.concat([train_data[['Id']],train_data.iloc[:,40:60]],axis=1)

### Na's to Nones function
def na_as_none(df, col_list):
    for column in col_list:
        df[column][df[column].isna()] = 'None'

### na's to nones
none_cols = ['FireplaceQu','GarageType']
na_as_none(train_subset,none_cols)

### replaced missing numericals with 0, imputed the most common class for KitchenQual and Functional, 0000 for missing GarageYrBlt to be imputed later

train_subset.BsmtFullBath[train_subset.BsmtFullBath.isna()] = 0
train_subset.BsmtHalfBath[train_subset.BsmtHalfBath.isna()] = 0
train_subset.KitchenQual[train_subset.KitchenQual.isna()] = 'TA'
train_subset.Functional[train_subset.Functional.isna()] = 'Typ'
train_subset.GarageYrBlt[train_subset.GarageYrBlt.isna()] = 0000

###

for column in train_subset.columns:
    print(column)
    print(train_subset[column].unique())

dummy_cols = ['HeatingQC','CentralAir','Electrical','GarageType','KitchenQual',
                'Functional','FireplaceQu']

dummy_drop_cols = ['TA','N','FuseP','TA','Typ','None']
