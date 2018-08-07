library(randomForest)
library(rsample)
library(ranger)
library(caret)
library(h2o)
### Machine Learning ###

setwd("/Users/JARVIS/RStudio/Capstone")

df <- read.csv("properties_2017_clean.csv")
prop.train.join <- tbl_df(df)

## Basic Random Forest Application

# Use set.seed for reproducability
set.seed(123)

## Prepare the training set ##
# after several trials, 10000 rows provided a relatively quick output time relative to the
# volume
pt.MLSet <- prop.train.join %>% 
  sample_n(10000) %>%
  
  # Remove the categorical factors
  select(-county_land_use_code, -date, -assessment_year)

# Create the training and testing sets
ptSplit <- initial_split(pt.MLSet, prop = .7)
pt_trainSet <- training(ptSplit)
pt_testSet <- testing(ptSplit)

## Random Forests for each feature set ##
# Start the random forest generation using ALL the features
RFM1 <- randomForest(
  formula = total_tax_value_dollars ~ .,
  data = pt_trainSet
)

# Plot the random forest generation
RFM1
plot(RFM1)

# Find the number of trees with the lowest mse
minTree <- which.min(RFM1$mse)
# Output: 121

# Using this info, derive the rmse
sqrt(RFM1$mse[minTree])
# Output: 158598.7

# Start the random forest generation using the following features
RFM2 <- randomForest(
  formula = total_tax_value_dollars ~ bath_count + bed_count + calc_finished_sqft,
  data = pt_trainSet
)

# Plot the random forest generation
RFM2
plot(RFM2)

# Find the number of trees with the lowest mse
minTree <- which.min(RFM2$mse)
# Output: 229

# Using this info, derive the rmse
sqrt(RFM2$mse[minTree])
# Output: 502320.4

