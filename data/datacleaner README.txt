Raw Data Cleaning READ ME

IMPUTATION
LotFrontage: MAR - Linear Regression with LotArea as Predictor, Outliers were removed
MSZoning: MAR - Values match with BldgType='1Fam' and HouseStyle='1Story. Imputed by Mode
BsmtUnfSF, TotalBsmtSF, BsmtHalfBath: Imputed 0, observations have no Basement
MasVnrType: Imputed 'None' where MasVnrArea was not missing
MasVnrType: Imputed Mode = 'BrkFace' where MasVnrArea was missing
MasVnrArea: Imputed Mean
Functional: Imputed Mode
Utilities: Imputed Mode <- Column has no variance
GargeCar: Imputed Mode where GarageType is Detchd
GarageArea: Imputed Mean where GarageType is Detchd
Electrical: Imputed Mode where YearBuilt > 1970
KitchenQual: Imputed Mode
SaleType: Imputed Mode
Exterior1st, Exterior2nd: Imputed Mode where RoofMatl == 'Tar&Grv'


REPLACEMENT: Using data_description.txt
BsmtCond: Imputed 'None'
BsmtQual: Imputed 'None'
BsmtExposure: Imputed 'None'
BsmtFinType2: Imputed 'None'
BsmtFinType1: Imputed 'None'
Alley: Imputed 'None'
MiscFeature: Imputed 'None'
Fence: Imputed 'None'
PoolQC: Imputed 'None'
FireplaceQu: Imputed 'None'
GarageCond: Imputed 'None'
GarageFinish: Imputed 'None'
GarageType: Imputed 'None'
GarageQual: Imputed 'None'
GarageYrBlt: Imputed 9999
BsmtUnfSF: Imputed 'None'
TotalBsmtSF: Imputed 'None'
BsmtHalfBath: Imputed 'None'
MSSubClass: Imputed 'None'
OverallCond: Imputed 'None'
OverallQual: Imputed 'None'


