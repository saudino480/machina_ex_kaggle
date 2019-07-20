library(dplyr)
library(tidyverse)

nested_ifelse = function(dfcol, list, i = 0) {
  ifelse(dfcol == list[i+1], i, nested_ifelse(dfcol, list, i+1))
}

