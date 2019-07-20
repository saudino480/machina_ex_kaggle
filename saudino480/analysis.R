library(tidyverse)
library(ggplot2)
library(dplyr)

housing = read.csv('./sam_train.csv')

cov(housing, housing$saleprice)
cols = columnnames(housing)

log_saleprice = log(housing$saleprice)

saleID = which(colnames(housing) == "saleprice")



housingPlotter = function(df, cols_idx, sale_id) {
  df_plot = df[,c(cols_idx, sale_id)]
  df_plot[,length(colnames(df_plot))] = log(df_plot[,length(colnames(df_plot))])
  plot(df_plot)
}

housingPlotter(housing, 51:60, saleID)

cols = c("x1stflrsf", "x2ndflrsf", "grlivarea", "totrmsabvgrd", "centralair_y", "masvnrarea", "totalbsmtsf", 
         "mssubclass", "lotarea", "overallqual", "neighborhood", "yearbuilt", "overallcond", "garagecars", 
         "garagearea", "openporchsf", "paveddrive_fy", "saletype_fcwd", "garagefinish_fnone", "fence_fnone",
         "fireplaces", "heatingqc_ex", "saleprice")


housing = housing %>%
  dplyr::select(cols)

write.csv(housing, './sam_train.csv')
