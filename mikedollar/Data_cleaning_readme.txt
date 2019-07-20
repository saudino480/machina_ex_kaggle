### Working only from the original dataset, train.csv


### Columns 1:20
-changed LotFrontage (numerical) NA's to 0
-changed Alley (character) NA's to 'None'

### Columns 21:40
-Changed all character-class Bsmt columns with shared NA's to 'None'.
-Changed all MasVnrType (character) NA's to 'None'
-changed all MasVnrArea (numerical) NA's to 0
-Had to change some incorrect values from BsmtFinType columns, but this was easily handled by looking at the neighboring columns.  The neighboring columns had information on square footage of finished or unfinished sections of basement.
-imputued 1 BsmtExposure MCAR value with the most frequently used category for that range of distrubution of TotalBsmtSF.  The value was 'Av'

### Columns 41:60
-changed GarageType (character) NA's to 'None'
-changed GarageYrBlt (character) NA's to 'None'
-changed FireplaceQu (character) NA's to 'None'

Handling the MCAR value from Electrical:
-Looking at the histogram of YearBuilt, I can surmise that pretty much anything built after 1970 is probably sporting the class, SBrkr for the column Electrical. So I imputed the missing value as 'SBrkr'

### Columns 61:80
-changed 'DtSold to ymd, and changed all days to 1, because I don't think the day sold is important.
The following features were changed conditional on related numerical columns with 0 as a shared entry.  For example, (GarageArea == 0) means no garage, garage finish, garage quality, etc.
-changed GarageCond (character) NA's to 'None'
-changed GarageFinish (character) NA's to 'None'
-changed GarageQual (character) NA's to 'None'
-changed PoolQC (character) NA's to 'None'
-changed Fence (character) NA's to 'None'
-changed MiscFeature (character) NA's to 'None'

