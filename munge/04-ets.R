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
library(ggplot2)
library(dplyr)

# Load data
train <- 
  read.csv("../data/prepped/train.csv", 
           stringsAsFactors = FALSE)

test <- 
  read.csv("../data/prepped/test.csv", 
           stringsAsFactors = FALSE)

# Not all stores in the training set that are in the test set.
# All stores in the test set are in the training set.
x <- unique(train$Store)
y <- unique(test$Store)
all(x %in% y)
all(y %in% x)

# Filter our stores that are in test but not in train
train <- train[train$Store %in% test$Store, ]

# Aggregate to daily store sales
train <- aggregate(Sales ~ Store + Date, train, FUN = sum)

# Munge data
train[, "Date"] <- as.Date(train[, "Date"])
test[, "Date"] <- as.Date(test[, "Date"])

# Check for NAs
lapply(train, function(.x) any(is.na(.x)))
lapply(test, function(.x) any(is.na(.x)))

# Make sales positive if it isn't
train[, "Sales"] <- ifelse(train[, "Sales"] < 0, 0, train[, "Sales"])

# Split data
train_list <- 
  split(train, list(factor(train$Store)))

test_list <- 
  split(test, list(factor(test$Store)))

# Function to handle making the forecast
make_forecast <- function(.train, .test, .count) {
  print(.count)
  # Handle train
  .train <- .train[, c("Date", "Sales")]
  .train_min_date <- min(as.Date(.train[, "Date"]))
  .train_max_date <- max(as.Date(.train[, "Date"]))
  
  .train_frame <- data.frame(Date = seq.Date(from = .train_min_date, 
                                             to = .train_max_date, 
                                             by = "day"), 
                             stringsAsFactors = FALSE)
  .train <- merge(.train_frame, .train, by = "Date")
  .train[, "Sales"] <- ifelse(is.na(.train[, "Sales"]), 0, .train[, "Sales"])
  
  # Handle test
  .test <- .test[order(.test[, "Date"]), ]
  .test <- .test[, "Date", drop = FALSE]
  .test_min_date <- min(as.Date(.test[, "Date"]))
  .test_max_date <- max(as.Date(.test[, "Date"]))
  .test_dates <- seq.Date(from = .test_min_date, 
                          to = .test_max_date, 
                          by = "day")
  
  # Build model
  .train_ts <- ts(.train[, "Sales"], start = .train_min_date, frequency = 7)
  .h <- length(.test_dates)
  .model <- forecast::ets(.train_ts)
  
  # Handle forecasts
  .forecast <- forecast::forecast(.model, h = .h)
  .predictions <- data.frame(Date = .test_dates, 
                             Sales = .forecast$mean, 
                             stringsAsFactors = FALSE)
  .predictions[, "Sales"] <- ifelse(.predictions[, "Sales"] < 0, 
                                    0, .predictions[, "Sales"])
  .test <- merge(.test, .predictions, all.x = TRUE)
  return(.test)
}

# Forecast data
t1 <- Sys.time()
forecast_list <- Map(make_forecast, 
                     train_list, test_list, 1:length(test_list))
t2 <- Sys.time(); t2 - t1

# Bring data back together
predictions <- do.call(rbind, forecast_list)

# Work with Store names
predictions[, "Store"] <- floor(as.numeric(rownames(predictions)))
rownames(predictions) <- NULL

# Reorder frame
predictions <- predictions[, c("Store", "Date", "Sales")]

# Create submission file
submission <- merge(test[, c("Id", "Store", "Date")], predictions, 
                    by = c("Store", "Date"))

submission <- submission[, c("Id", "Sales")]
submission <- submission[order(submission[, "Id"]), ]
rownames(submission) <- NULL

# Write data out to /data/prepped
write.csv(submission, "../data/prepped/submission-ets.csv", 
          row.names = FALSE)
