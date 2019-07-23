
import matplotlib.pyplot as plt
plt.style.use('ggplot')
import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn import datasets
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.feature_selection import SelectFromModel
from sklearn.metrics import accuracy_score, mean_squared_error

def to_numeric(df, col, target="", test = False):
	"""Refactors levels to numerals in order of increasing median sale price

	Keyword arguments:
	df -- dataframe
	col -- column present in dataframe
	target -- column to aggregate median (default "")
	test -- unknown (default False)

	Returns:
	df -- Updated dataframe with ordinated column
	id_dict -- Key-Change Dictionary for altered column
	"""
	group = df.groupby(col)
	group = group.aggregate({ target :'median'})
	group = group.sort_values(target)

	names = list(group.index)
	#print(names)
	id_dict = {}
	i = 0
	for name in names:
		#print(name)
		id_dict.update({name: i})
		i += 1
	#print("Encoded values for: ", col)
	#print(id_dict)
	#print(df[col].unique())
	df[col] = [id_dict[x] for x in df[col]]

	return df, id_dict

def to_numeric_test(df, col, id_dict):
	"""Refactors levels to numerals in order of according to key-change dictionary
	
	Keyword arguments:
	df -- dataframe
	col -- column present in dataframe
	id_dict -- key-change dictionary 

	Returns:
	df -- Updated dataframe with ordinated column
	id_dict -- Key-Change Dictionary for altered column
	"""


	dict_keys = list(id_dict.keys())

	names = list(df[col].unique())
    #print(col, "*"*50)
    #print(dict_keys)
    #print(names)
	missing_values = [x for x in names if x not in dict_keys]
    
    #print(missing_values)
	i = len(dict_keys) // 2
	for name in missing_values:
        #print(name)
		id_dict.update({name: i})
    #print("Encoded values for: ", col)
    #print(id_dict)
    #print(df[col].unique())
	df[col] = [id_dict[x] for x in df[col]]

	return df, id_dict

def run_linear_model(df, feat = [], target='prices',split=0.33,model=LinearRegression):
	"""Runs a linear model on selected features from a dataset

	Keyword Arguments:
	df 		-- data frame containing all the training data (default train)
	feat 	-- list of the names of features to model against (default []_)
	target 	-- name of column contianing target data to predict (default 'prices')
	split 	-- fraction of total dataset to leave for testing, must be less than 1.0 (default 0.33)
	model 	-- function to call linear model (default LinearRegression)
	params  -- list of parameters that will be used by models

	Returns:

	"""
	

	## Split Model into Train/Test
	fTrain, fTest, pTrain, pTest = train_test_split(df.loc[:,feat], target, test_size = split, random_state = 42)


	## Run Models on Training subset, then Predict on the Testing subset
	if (model == RandomForestRegressor):
		# Instantiate RandomForestRegressor
		clf = RandomForestRegressor(n_estimators=10000, random_state=0, n_jobs=-1, min_samples_split = 10)
		sfm = SelectFromModel(clf, threshold = 0.005)
		sfm.fit(fTrain, pTrain)

		## Not Finished
		# Measure Feature Importance
		feature_selected = []
		for feature_list_index in sfm.get_support(indices=True):
		    feature_selected.append(feat_labels[feature_list_index])
		proxy = feature_selected
		trimmed = ['x1stflrsf', 'x2ndflrsf', 'garagecars', 'overallcond', 'saleprice', 'Unnamed: 0', 'bsmtfinsf1']
		housing_features = housing_features[proxy]
		housing_features = housing_features[trimmed_features]

	elif (model == LinearRegression):
		lm = LinearRegression()
		cvs = np.mean(cross_val_score(lm, fTrain, pTrain, cv=5))
		lm.fit(fTrain, pTrain)
		pPred = lm.predict(fTest)
		mse = mean_squared_error(pTest, pPred)
		results = np.exp(pPred)
	


	print('Model: ', model)
	print('Features: ', feat)
	print('MSE: ', mse)
	print('CVS: ', cvs)



	## Generates Submission File for Kaggle
	#submission = pd.DataFrame(columns = ['Id', 'SalePrice'])
	#submission['Id'] = housing_test.id
	#submission['SalePrice'] = results
	#submission.to_csv('submission.csv', index=False)

