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

# Not all stores in the training set that are in the test set.
# All stores in the test set are in the training set.
x <- unique(train$Store)
y <- unique(test$Store)
all(x %in% y)
all(y %in% x)

# Filter our stores that are in test but not in train
train <- train[train$Store %in% test$Store, ]

# Aggregate to daily store sales
train <- aggregate(Sales ~ Store + Date + DayOfWeek, train, FUN = sum)

# Munge data
train[, "Date"] <- as.Date(train[, "Date"])
test[, "Date"] <- as.Date(test[, "Date"])

# Check for NAs
lapply(train, function(.x) any(is.na(.x)))
lapply(test, function(.x) any(is.na(.x)))

# Make sales positive if it isn't
train[, "Sales"] <- ifelse(train[, "Sales"] < 0, 0, train[, "Sales"])

# Split data
median_sales_by_store_day <- 
  aggregate(Sales ~ Store + DayOfWeek, train, FUN = median)

# Create submission file
submission <- merge(test[, c("Id", "Store", "DayOfWeek")], 
                    median_sales_by_store_day, 
                    by = c("Store", "DayOfWeek"))

submission <- submission[, c("Id", "Sales")]
submission <- submission[order(submission[, "Id"]), ]
rownames(submission) <- NULL

# Write data out to /data/prepped
write.csv(submission, "../data/prepped/submission-median-seasonal.csv", 
          row.names = FALSE)
