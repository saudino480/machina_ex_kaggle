
import matplotlib.pyplot as plt
plt.style.use('ggplot')
import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression, LassoCV, Lasso, Ridge, ElasticNet
from sklearn.ensemble import RandomForestRegressor
from sklearn import datasets
from sklearn.model_selection import train_test_split, cross_val_score, KFold, GridSearchCV
from sklearn.feature_selection import SelectFromModel
from sklearn.metrics import accuracy_score, mean_squared_error



def plot_model_accuracy(df):
	fig, ax1 = plt.subplots()
	#ax1.plot(df.index, df['Train Score'],label = 'Train Score')
	ax1.plot(df.index, df['Test Score'],label='Test Score')
	ax1.set_xlabel('Model')
	plt.xticks(rotation=40)
	ax1.set_ylabel('R2')

	ax2 = ax1.twinx()
	ax2.plot(df.index, df['Kaggle Metric'], label = 'Kaggle Score',color='blue')
	ax2.set_ylabel('RMSLE')
	plt.title('Model Fitness on Test Set')
	plt.legend(loc=1)
	plt.show()

def rmsle(real, predicted):
	sum=0.0
	for x in range(len(predicted)):
		if predicted[x]<0 or real[x]<0: #check for negative values
			continue
		p = (predicted[x]+1)
		r = (real[x]+1)
		sum = sum + (p - r)**2
	return (sum/len(predicted))**0.5



def modelStack(training, testing, target, produce_submission = False, n_splits = 5):
	X_train, X_test, Y_train, Y_test = train_test_split(training, target, test_size = 0.2, random_state=42)

	skf = KFold(n_splits, random_state = None, shuffle = True)
	skf.get_n_splits(X_train, Y_train)

	X_train_set = []
	X_test_set = []
	y_train_set = []
	y_test_set = []
	#index_train_set = []
	#index_test_set = []

	for train_index, test_index in skf.split(X_train, Y_train):
		#print("TRAINING:", train_index, "TEST:", test_index)
		#index_train_set.append(train_index)
		#index_test_set.append(test_index)
		X_train_set.append(X_train.iloc[train_index])
		X_test_set.append(X_train.iloc[test_index])
		y_train_set.append(Y_train.iloc[train_index])
		y_test_set.append(Y_train.iloc[test_index])

	#index_train_set = [x.tolist() for x in index_train_set]

	#print(len(X_train_set))
	pred_train_dict = {}
	pred_test_dict = {}
	predict_final = {}

	alpha_steps = (1e-5,1e-2,200)
	steps = np.linspace(1,0.8,21)
	lasso = Lasso()
	ridge = Ridge()
	elasticnets = [ElasticNet(l1_ratio = i) for i in steps[1:]]
	names = ['Lasso'] + ['EN_' + str(round(i,2)) for i in steps[1:]]
	modelList = pd.Series([lasso] + elasticnets, index = names)

	for i in range(len(X_train_set)):
		print(i)
		print(X_train_set[i].shape)
		print('Tuning Hyperparameters...')
		param_grid = {'max_iter': [10,100,1000],
					'alpha':np.linspace(1e-5,1e-3,100)}
		grid = GridSearchCV(modelList.Lasso,param_grid,scoring='r2',cv=5)
		grid.fit(X_train_set[i], y_train_set[i])
		best_alpha = grid.best_params_['alpha']
		best_iter = grid.best_params_['max_iter']
		print('Best Alpha Found: {}\n'.format(best_alpha))
		modelList.apply(lambda x: x.set_params(alpha = best_alpha, max_iter = best_iter))

		modelList.apply(lambda x: x.fit(X_train_set[i], y_train_set[i]))
		train_errors, test_errors = [], []
		train_errors.append(modelList.apply(lambda x: x.score(X_train_set[i], y_train_set[i])))
		test_errors.append(modelList.apply(lambda x: x.score(X_train_set[i], y_train_set[i])))
		scores_df = pd.DataFrame({"Train Score":modelList.apply(lambda x: x.score(X_train_set[i], y_train_set[i])),
						 "Test Score": modelList.apply(lambda x: x.score(X_train_set[i], y_train_set[i])),
						 "Kaggle Metric": modelList.apply(lambda x: rmsle(y_test_set[i].values, x.predict(X_test_set[i])))
						})

		plot_model_accuracy(scores_df)
		np.argmin(scores_df['Kaggle Metric'])
		print('Best Model: ', np.argmin(scores_df['Kaggle Metric']))
		print('Kaggle Score: ', round(min(scores_df['Kaggle Metric']),4))

		print('')
		model = modelList[np.argmin(scores_df['Kaggle Metric'])].fit(X_train_set[i], y_train_set[i])

		#model.fit(X_train_set[i], y_train_set[i])
		for j in range(i + 1, len(X_train_set)):
			print(j)
			pred_train = model.predict(X_train_set[j])
			pred_test = model.predict(X_test_set[j])
			df_train = pd.DataFrame(pred_train, index = X_train_set[j].index)
			df_test = pd.DataFrame(pred_test, index = X_test_set[j].index)
			#df_final = pd.DataFrame(model.predict(training), index = X_train_set[j].index)
			#print(df.index == X_train_set[j].index)
			#print((X_train_set[j].head()))
			if (j == (i+1)):
				df_X_test = pd.DataFrame(model.predict(X_test), index = X_test.index)
				df_training = pd.DataFrame(model.predict(training), index = training.index)
				df_testing = pd.DataFrame(model.predict(testing), index = testing.index)

				print(mean_squared_error(Y_test, model.predict(X_test)))

				X_test = pd.concat([X_test, df_X_test], axis = 1, join_axes = [X_test.index])
				training = pd.concat([training, df_training], axis = 1, join_axes = [training.index])
				testing = pd.concat([testing, df_testing], axis = 1, join_axes = [testing.index])

			X_train_set[j] = pd.concat([X_train_set[j], df_train], axis = 1, join_axes = [X_train_set[j].index], join = 'inner')
			X_test_set[j] = pd.concat([X_test_set[j], df_test], axis = 1, join_axes = [X_test_set[j].index], join = 'inner')
			print((X_train_set[j]).shape)


	if (produce_submission):
		return np.exp(model.predict(testing))




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
	#print(col, "*"*50)
	#print(dict_keys)
	#print(names)
	missing_values = [x for x in names if x not in dict_keys]

	#print(missing_values)
	i = len(dict_keys) // 2
	for name in missing_values:
		#print(name)
		local_dict.update({name: i})
	#print("Encoded values for: ", col)
	#print(local_dict)
	#print(df[col].unique())
	df[col] = [local_dict[x] for x in df[col]]

	return df, local_dict


def read_and_clean(filepath, test = False):
	if (test):
		#### Read Data files
		id_housing = pd.read_csv(filepath)
		housing = id_housing.drop('Id', axis=1)
		### Identifies columns by type obj
		#needs_numeric = housing.loc[:, housing.dtypes == "object"]
		#colname = list(needs_numeric.columns)
		needs_tranform = housing.loc[:, housing.dtypes != 'object']
		#num_colname = list(needs_numeric.columns)
		trans_colname = list(needs_tranform.columns)
		trim = ['BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'HalfBath',
				'BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces',
				'GarageYrBlt', 'GarageCars', 'MiscVal', 'MoSold', 'YrSold',
				'MSSubClass']
		trans_colname = [x for x in trans_colname if x not in trim]
		#print(trans_colname)
		### Process Datafiles for Modelling
		for col in trans_colname:
			if (col == 'SalePrice'):
				housing[col] = np.log(housing[col])
			else:
				housing[col] = np.sqrt(housing[col])
		#dict_dictonary = {}

		housing.MSSubClass = housing.MSSubClass.astype(str)

		housing = pd.get_dummies(housing)
		#for col in num_colname:
		#	housing.col, id_dictonary = to_numeric_test(housing, col, dictonary)
		#	dict_dictonary.update({col : id_dictonary})
			#print(id_dictonary[col], " and ", col)
		housing.columns = housing.columns.str.lower()
		housing_features = housing
		feat_labels = housing_features.columns
		#housing.saleprice = np.log(housing.saleprice)
		return id_housing.Id, housing_features, feat_labels#, dict_dictonary
	else:
		#### Read Data files
		housing = pd.read_csv(filepath)
		outliers = [198,  524,  589,  692,
					826, 1183, 1299, 1424]
		housing = housing[~housing["Id"].isin(outliers)]
		housing = housing.drop('Id', axis=1)
		### Identifies columns by type obj
		#needs_numeric = housing.loc[:, housing.dtypes == "object"]
		needs_tranform = housing.loc[:, housing.dtypes != 'object']
		#num_colname = list(needs_numeric.columns)
		trans_colname = list(needs_tranform.columns)
		trim = ['BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'HalfBath',
				'BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces',
				'GarageYrBlt', 'GarageCars', 'MiscVal', 'MoSold', 'YrSold',
				'MSSubClass']
		trans_colname = [x for x in trans_colname if x not in trim]
		#print(trans_colname)
		### Process Datafiles for Modelling
		for col in trans_colname:
			if (col == 'SalePrice'):
				housing[col] = np.log(housing[col])
			else:
				housing[col] = np.sqrt(housing[col])

		housing.MSSubClass = housing.MSSubClass.astype(str)
		housing['MSSubClass_150'] = 0
		housing = pd.get_dummies(housing)
		#dict_dictonary = {}
		#for col in num_colname:
		#	housing.col, id_dictonary = to_numeric(housing, col, 'SalePrice')
		#	dict_dictonary.update({col : id_dictonary})
		housing.columns = housing.columns.str.lower()
		housing_features = housing.drop(['saleprice'], axis=1)
		feat_labels = housing_features.columns
		#housing.saleprice = np.log(housing.saleprice)

		return housing, housing_features, feat_labels#, dict_dictonary




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
