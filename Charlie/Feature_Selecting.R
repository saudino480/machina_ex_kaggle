library(car)
library(dplyr)

train <- read.csv('Charlie/datafiles/train_cc_clean.csv')
test <- read.csv('Charlie/datafiles/test_cc_clean.csv')


colSums(is.na(test))

train

## Dummification Test
model.dummied = lm(SalePrice ~ MSZoning + Street + Neighborhood, data = train)
summary(model.dummied)


# Full Model
set.seed(0)
train_split = sample(1:nrow(train), 5*nrow(train)/10)
train_test = train[-train_split,]

model.full = lm(SalePrice ~ ., data = train, subset = train_split)
summary(model.full)

pred = predict(model.full, train_test)


# Check categorical significance
for (i in names(total)) {
  if (is.factor(total[,i])) {
    print(paste('*****',i,'******'))
    print((merge(data.frame(table(train[,i])), data.frame(table(test[,i])),by = 'Var1',all=T)))
  }
}

table(train[,'Heating'])
