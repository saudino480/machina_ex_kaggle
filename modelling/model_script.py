#### Read Data files
dict_dictonary = {}
housing, housing_features, feat_labels, dict_dictonary = read_and_clean(filepath = "../data/clean_train.csv")
htest_id, htest_features, htest_labels, htest_dict = read_and_clean(filepath = "../data/clean_test.csv",
                                                                    test = True,
                                                                    dictonary = dict_dictonary)

training = housing_features
testing = htest_features

#### Process and Generate Train Test Splits
test_col = testing.columns
train_col = training.columns

missing = [x for x in train_col if x not in test_col]
needed = [x for x in test_col if x not in train_col]

training = training.drop(missing, axis=1)
testing = testing.drop(needed, axis=1)

X_train, X_test, Y_train, Y_test = train_test_split(training, housing.saleprice, test_size = 0.2, random_state=42)
