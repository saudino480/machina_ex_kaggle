library(car)
library(dplyr)
library(caret)


train <- read.csv('data/clean_train.csv')
test <- read.csv('data/clean_test.csv')


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
    SalesMed = train %>% group_by_(i) %>% dplyr::summarise(t = median(SalePrice),m = mean(SalePrice),sd = sd(SalePrice))
    df <- (merge(data.frame(table(train[,i])), data.frame(table(test[,i]),SalesMed),by = 'Var1',all=T))
    df <- df[,-4]
    names(df) <- c('Name','Train','Test','SalesMed','SalesMean','SalesSD')
    print(df %>% arrange(SalesMed))
  }
}

table(train[,'Heating'])





### Binning Categorical Columns
total = rbind(train %>% select(-SalePrice),test)

train %>% group_by_(i) %>% dplyr::summarise(m = mean(SalePrice))

train$Alley

for (i in names(total)){

}
ggplot(data = train) +
  geom_bar(aes(x = Neighborhood,fill=Neighborhood))
