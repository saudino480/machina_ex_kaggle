library(dplyr)
df1 <- read.csv('clean_data/sam_slice_clean.csv')
df2 <- read.csv('clean_data/mike_slice_clean.csv')
df3 <- read.csv('clean_data/mikejr_slice_clean.csv')
df4 <- read.csv('clean_data/charlie_slice_clean.csv')
df5 <- read.csv('../data/train.csv')

correct_names <- function(df) {
  names(df) <- gsub('\\.','_',names(df)) # I took out the tolower function so the columns are more readable.
  return (df) }



df1 = correct_names(df1)
df2 = correct_names(df2)
df3 = correct_names(df3)
df4 = correct_names(df4)
df5 = correct_names(df5)

df5 = df5 %>% select(Id, SalePrice)




dfout = bind_cols(list(df1,df2,df3,df4,df5))
dfout = dfout %>% select(-Id1,-Id2,-Id3,-Id4)

write.csv(dfout,'clean_data/train_clean.csv',row.names = F)
