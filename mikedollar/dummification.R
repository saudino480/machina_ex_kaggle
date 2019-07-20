# dummify categorical variables from cleaned dataset

cdf <- read.csv('../data/cleaned_dataset_train.csv',stringsAsFactors = F)




# change date column to datetime format
library(lubridate)
library(dplyr)
cdf$dtsold = as_date(cdf$dtsold)

# I noticed that the date sold is always on the first of the month, so I will remove
# the dtsold column and make two other columns for the year and the month.
cdf = cdf %>% 
  mutate(., dtsold_year = year(dtsold), dtsold_month = month(dtsold)) %>% 
  select(., -dtsold)

# get list of categorcal variables
catlist = colnames(cdf[, sapply(cdf, class) == 'character'])

# dummify the variables in catlist

cdf = dummy_cols(cdf, remove_first_dummy = T)


# drop columns in catlist
cdf = cdf %>%
  select(., -catlist)

write.csv(cdf, 'full_dummied.csv', row.names = F)
