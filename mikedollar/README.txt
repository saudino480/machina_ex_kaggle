### clean_21_40.R
-This file cleans columns 21 to 40.  It is now obsolete, due to the addition of data_cleaning_all.R

### dummification.R
-dummifies all variables.

*** This file will be changed, since the dummification was based on the wrong dataset.

### data_cleaning_all.R
- cleans entire dataset, imputes missing values, and corrects some caught incorrect values.
- cleaned datasets are named and stored in directory: machina_ex_kaggle/mikedollar/clean_data

*** details are in Data_cleaning_readme.txt

### regression.ipynb
- scratchpad for exploring regression models in python using the housing data.

### dataset_merge.R
- merges all cleaned datasets from the different row divisions.  Specifically merges on the Id number.

*** original file is located in directory: machina_ex_kaggle.  This version has been modified to leave capital letters as they were, and the the file paths for each cleaned dataset section were updated.


