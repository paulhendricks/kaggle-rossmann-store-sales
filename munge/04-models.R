###############################################################################
# TO BE EDITED
#
# Authors:
# Paul Hendricks
#
# Date:
# 11-06-2015
#
# inputs:
# foo.csv
#
# outputs:
# bar.RData
###############################################################################

# Load libraries
library(forecast)

# Load data
train <- 
  read.csv("../data/prepped/train.csv", 
           stringsAsFactors = FALSE)

test <- 
  read.csv("../data/prepped/test.csv", 
           stringsAsFactors = FALSE)

store <- 
  read.csv("../data/prepped/store.csv", 
           stringsAsFactors = FALSE)

# Merge data
train_list <- 
  split(train, list(factor(train$Store)))

