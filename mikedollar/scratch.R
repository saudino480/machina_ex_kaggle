train_clean <- read.csv('clean_data/train_clean.csv', stringsAsFactors = F)

library(ggplot2)
library(dplyr)

train_clean %>% 
  group_by(., Neighborhood) %>% 
  summarise(., count(MSZoning))

ggplot(data = train_clean %>% group_by(Neighborhood), aes(x = Neighborhood, fill = MSZoning))+
  geom_histogram(stat='count')


View(train_clean %>% select(Street, SalePrice))

ggplot(data = train_clean %>% select(Street, SalePrice), aes(x = SalePrice, fill = Street))+
  geom_histogram(stat='count')


test <- read.csv('../data/cleaned_test.csv')

library(mice)

ggplot(data=train_clean, aes(x=LotFrontage[which()], y=LotArea))+
  geom_point()


idx = which((train_clean$LotFrontage != 0) & (train_clean$LotArea < 15000))


model = lm(LotFrontage[idx] ~ LotArea[idx], data = train_clean)


beta_0 = model$coefficients[1]
beta_1 = model$coefficients[2]

for (i in 1:length(train_clean$LotFrontage)){
  if(train_clean$LotFrontage[i] == 0){
    train_clean$LotFrontage[i] = beta_0 + beta_1*train_clean$LotArea[i]
  }
}


View(train_clean %>% 
  select(., contains('Garage')))
