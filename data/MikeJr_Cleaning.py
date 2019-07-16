### packages
import numpy as np
import pandas as pd

### load data
train_data = pd.read_csv('./data/test.csv')
### subset data
train_subset = pd.concat([train_data[['Id']],train_data.iloc[:,40:60]],axis=1)

#
train_subset.isna().sum()

### Na's to Nones function
def na_as_none(df, col_list):
    for column in col_list:
        df[column][df[column].isna()] = 'None'

# na's to nones
none_cols = ['FireplaceQu','GarageType']
na_as_none(train_subset,none_cols)

#
train_subset[['FireplaceQu']]
