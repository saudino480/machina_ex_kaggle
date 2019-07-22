library(tidyverse)
library(dplyr)
library(ggrepel)
library(stringr)
library(VIM)

source('./data/helper.R')

# This assumes that you are in a named folder in the root directory of the
# GitHub project.


train_output = full_clean(read_path = './data/train.csv', write_path = './data/cleaned_train.csv', dummies = FALSE)

test_output = full_clean('./data/test.csv', './data/cleaned_test.csv', dummies = FALSE)


