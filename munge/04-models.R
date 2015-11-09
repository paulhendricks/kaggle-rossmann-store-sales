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

# Split data
train_list <- 
  split(train, list(factor(train$Store)))

test_list <- 
  split(test, list(factor(test$Store)))

# Cast data to time series
cast_df_to_ts <- function(.df) {
  start_date <- min(as.Date(.df[, "Date"]))
  return(ts(.df[, "Sales"], start = start_date, frequency = 7))
}

train_ts_list <- lapply(train_list, cast_df_to_ts)
# test_ts_list <- lapply(test_list, cast_df_to_ts)

# Model data
model_list <- lapply(train_ts_list, forecast::ets)

# Forecast data
forecast_list <- lapply(model_list, forecast::forecast)

# Accuracy
accuracy_list <- lapply(model_list, forecast::accuracy)
