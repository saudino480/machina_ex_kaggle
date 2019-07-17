### packages
import numpy as np
import pandas as pd
import csv

### load data
train_data = pd.read_csv('../data/test.csv')
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

### dummify and drop redundancies

dummy_cols = ['HeatingQC','CentralAir','Electrical','GarageType','KitchenQual',
                'Functional','FireplaceQu']
dummy_drop_cols = ['HeatingQC_TA','CentralAir_N','Electrical_FuseA','KitchenQual_TA',
                    'Functional_Typ','FireplaceQu_None','GarageType_None']

dummies_df = pd.get_dummies(train_subset[dummy_cols])

dummy_drop_cols.extend(dummy_cols)

train_subset = pd.concat([train_subset,dummies_df],axis=1).drop(dummy_drop_cols,axis=1)

### write csv

with open('mikejr_clean.csv',mode='w') as file:
    data_writer = csv.writer(file,delimiter=',')

    data_writer.writerow(train_subset.columns.tolist())

    for row in range(len(train_subset)):
        data_writer.writerow(train_subset.iloc[row].tolist())
