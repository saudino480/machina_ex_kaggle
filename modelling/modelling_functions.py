
import matplotlib.pyplot as plt
plt.style.use('ggplot')
import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression, LassoCV, Lasso, Ridge
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

	local_dict = id_dict[col]

	dict_keys = list(local_dict.keys())

	names = list(df[col].unique())
	print(col, "*"*50)
	print(dict_keys)
	print(names)
	missing_values = [x for x in names if x not in dict_keys]

	print(missing_values)
	i = len(dict_keys) // 2
	for name in missing_values:
		#print(name)
		local_dict.update({name: i})
	#print("Encoded values for: ", col)
	print(local_dict)
	print(df[col].unique())
	df[col] = [local_dict[x] for x in df[col]]

	return df, local_dict


def read_and_clean(filepath, test = False, dictonary = {}):
	if (test):
		#### Read Data files
		id_housing = pd.read_csv(filepath)
		housing = id_housing.drop('Id', axis=1)
		### Identifies columns by type obj
		needs_numeric = housing.loc[:, housing.dtypes == "object"]
		colname = list(needs_numeric.columns)
		### Process Datafiles for Modelling
		dict_dictonary = {}
		for col in colname:
			housing.col, id_dictonary = to_numeric_test(housing, col, dictonary)
			dict_dictonary.update({col : id_dictonary})
			#print(id_dictonary[col], " and ", col)
		housing.columns = housing.columns.str.lower()
		housing_features = housing
		feat_labels = housing_features.columns
		#housing.saleprice = np.log(housing.saleprice)
		return id_housing.Id, housing_features, feat_labels, dict_dictonary
	else:
		#### Read Data files
		housing = pd.read_csv(filepath).drop('Id', axis=1)
		### Identifies columns by type obj
		needs_numeric = housing.loc[:, housing.dtypes == "object"]
		colname = list(needs_numeric.columns)
		print(colname)
		### Process Datafiles for Modelling
		dict_dictonary = {}
		for col in colname:
			housing.col, id_dictonary = to_numeric(housing, col, 'SalePrice')
			dict_dictonary.update({col : id_dictonary})
		housing.columns = housing.columns.str.lower()
		housing_features = housing.drop(['saleprice'], axis=1)
		feat_labels = housing_features.columns
		#housing.saleprice = np.log(housing.saleprice)
		return housing, housing_features, feat_labels, dict_dictonary




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

		return lm, results

	elif (model == Lasso):
		lasso = Lasso()

		lasso.fit(housing_train, price_train)

		#print(lasso.score(housing_train,price_train))
		#print(lasso.score(housing_test, price_test))
		selected_coefs = list(np.where(lasso.coef_ != 0))[0]
		#print(selected_coefs)

		return lasso, selected_coefs

	print('Model: ', model)
	print('Features: ', feat)
	print('MSE: ', mse)
	print('CVS: ', cvs)


def Submission(df_id, results, filename="submission.csv"):
	## Generates Submission File for Kaggle
	submission = pd.DataFrame(columns = ['Id', 'SalePrice'])
	submission['Id'] = df_id
	submission['SalePrice'] = results
	submission.to_csv(filename, index=False)

def optimize_penalty(features, target, model = Lasso, min_=0,max_=10, step=0.01, plot=True):
	"""
	Finds the best setting for the penalty term in Regularized Regression
	Keyword Args:
	model     -- Which model to to run (default = Lasso)
	min_      -- min value to test (default = 0)
	max_      -- max value to test (default = 10)
	step      -- step size (default = 0.01)

	Returns:
	coefs_    -- list of model coefficients
	alphas-   -- list of alpha sizes
	R2_       -- list of R^2 scores
	"""
	coefs_ = []
	term_ = []
	R2_ = []
	md = model()
	features_train, features_test, price_train, price_test = train_test_split(features, target, test_size = 0.2)
	for t in np.arange(min_,max_,0.01):
		md.set_params(alpha=t)
		md.fit(features, target)
		coefs_.append(md.coef_)
		term_.append(t)
		R2_.append(md.score(features_test, price_test))

	if plot == True:
		plt.plot(term_,R2_,c='b',label=r'$R^2$')
		plt.title(r'$R^2$ v Regularization Penalty')
		plt.xlabel('Penalty Term')
		plt.ylabel(r'$R^2$')
		plt.legend(loc=0)
		plt.show()

	return coefs_, term_, R2_

def undo_transform(train, test, col, transform):
    if (transform == "sqrt"):
        train[col] = train[col]**2
        test[col] = test[col]**2
    elif (transform == "log"):
        train[col] = np.exp(train[col])
        test[col] = np.exp(test[col])
    #return train[col], test[col]
