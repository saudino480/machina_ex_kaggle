library(dplyr)
library(ggplot2)
library(car)
library(ISLR)
library(glmnet)
library(caret)


train = read.csv('Documents/machina_ex_kaggle/data/clean_train.csv')

# train = train[,-ncol(train)]
test = read.csv('Documents/machina_ex_kaggle/data/clean_test.csv')


full_df = train
garage_yrs = c(full_df$GarageYrBlt %>% unique() %>% sort())[-length(unique(full_df$GarageYrBlt))]
full_df$GarageYrBlt[full_df$GarageYrBlt==9999] = sample(garage_yrs,length(full_df$GarageYrBlt[full_df$GarageYrBlt==9999]),replace = T)

# full_df = cbind(full_df,c(saleprice,rep(NA,nrow(test))))
# colnames(full_df)[81] = 'SalePrice'

numerics= full_df[,!sapply(full_df,is.factor)]
numerics %>% colnames()


saleprice = train[,ncol(train)]
categoricals = cbind(full_df[,sapply(full_df,is.factor)],saleprice)
categoricals %>% colnames()

scatterplotMatrix(numerics)

# NUMERICS ####

lm.fit = lm(SalePrice ~.,data=numerics)


vif(lm.fit)


alias(lm.fit)

which(colnames(train)=='TotalBsmtSF')
which(colnames(train)=='GrLivArea')
collinear_cols = c(39,47)

full_df = full_df[,-collinear_cols]
numerics= full_df[,!sapply(full_df,is.factor)]

lm.fit = lm(SalePrice ~.,data=numerics)

vif(lm.fit)[which(vif(lm.fit)>5)]

hi_vif_cols = c('X2ndFlrSF','GarageCars','BsmtFinSF1')
hi_vif_cols = which(colnames(full_df) %in% hi_vif_cols)
full_df = full_df[,-hi_vif_cols]

numerics= full_df[,!sapply(full_df,is.factor)]

lm.fit = lm(SalePrice ~.,data=numerics)

vif(lm.fit)[which(vif(lm.fit)>5)]

influencePlot(lm.fit)



# vif(lm.fit)
# lm.fit = lm(SalePrice ~.-GarageCond-GarageQual-GarageFinish,data=full_df)
# alias(lm.fit)
# 
# unequals = which(full_df$GarageCond != full_df$GarageQual)
# 
# full_df$GarageCond[unequals]
# full_df$GarageQual[unequals]
# 
# par(mfrow=c(1,1))
# full_df$GarageYrBlt %>% unique() %>% sort()
# plot(full_df$GarageYrBlt)
# 
# lm.fit = lm(SalePrice ~.-GarageCond-GarageQual-GarageFinish-PoolArea,data=full_df)
# vif(lm.fit)^2

# CATEGORICALS ####

lm.fit.cat = lm(saleprice~.,data=categoricals)

vif(lm.fit.cat)
alias(lm.fit.cat)

collinear_cats = c('GarageQual','GarageCond','GarageFinish','HouseStyle','Exterior2nd')
collinear_cats= which(colnames(full_df) %in% collinear_cats)
full_df = full_df[,-collinear_cats]

categoricals = cbind(full_df[,sapply(full_df,is.factor)],saleprice)

lm.fit.cat = lm(saleprice~.,data=categoricals)
(vif(lm.fit.cat)^2)[,3][which((vif(lm.fit.cat)^2)[,3]>5)]

# NUMS & CATS ####

lm.fit.full = lm(SalePrice~.,data=full_df)
vif(lm.fit.full)
(vif(lm.fit.full)^2)[,3][which((vif(lm.fit.full)^2)[,3]>5)]

which(colnames(full_df)=='PoolQC')

full_df = full_df[,-which(colnames(full_df)=='PoolQC')]

lm.fit.full = lm(SalePrice~.,data=full_df)
vif(lm.fit.full)
(vif(lm.fit.full)^2)[,3][which((vif(lm.fit.full)^2)[,3]>5)]

# OUTLIERS ####

cookd = cooks.distance(lm.fit.full)
sample_size <- nrow(full_df)
plot(cookd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cookd)+1, y=cookd, labels=ifelse(cookd>4/sample_size, names(cookd),""), col="red")  # add labels

outliers = cookd[cookd>0.05 & !is.na(cookd)]
outliers = as.numeric(outliers %>% names())
full_df = full_df[-outliers,]

lm.fit.full = lm(SalePrice~.,data=full_df)

cookd = cooks.distance(lm.fit.full)
outliers = cookd[cookd>0.05 & !is.na(cookd)]
outliers = as.numeric(outliers %>% names())
full_df = full_df[-outliers,]

lm.fit.full = lm(SalePrice~.,data=full_df)

# LASSO ####
full_df = cbind(data.frame(scale(numerics)),categoricals)
categoricals = full_df[,sapply(full_df,is.factor)]     
                
x = model.matrix(SalePrice~.,full_df)[,-ncol(full_df)]
y = full_df$SalePrice

set.seed(0)
train = sample(1:nrow(x), 7*nrow(x)/10)
test = (-train)
y.test = y[test]


grid = 10^seq(5, -2, length = 100)

train_control = trainControl(method = 'cv', number=10)
tune.grid = expand.grid(lambda = grid, alpha=c(1))
lasso.caret = train(x[train, ], y[train],
                    method = 'glmnet',
                    trControl = train_control, tuneGrid = tune.grid)


pred = predict.train(lasso.caret, newdata = x[test,])
mean((pred - y[test])^2)


lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)




