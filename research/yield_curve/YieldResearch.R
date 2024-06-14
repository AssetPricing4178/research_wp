# Set working directory
setwd("C:/Users/Jonat/Desktop/Paper")

# Load necessary libraries
# install.packages("tidyverse")
library(tidyverse)
# install.packages("lubridate")
library(lubridate)
# install.packages("zoo")
library(zoo)

# Load the data
gs10 <- read.csv("GS10.csv")
gs2 <- read.csv("GS2.csv")
recession <- read.csv("USRECD.csv")
unrate <- read.csv("UNRATE.csv")
indpro <- read.csv("INDPRO.csv")

# Convert dates to appropriate format and select relevant columns
gs10 <- gs10 %>% mutate(date = as.Date(DATE)) %>% select(date, GS10)
gs2 <- gs2 %>% mutate(date = as.Date(DATE)) %>% select(date, GS2)
recession <- recession %>% mutate(date = as.Date(DATE)) %>% select(date, USRECD)
unrate <- unrate %>% mutate(date = as.Date(DATE)) %>% select(date, UNRATE)
indpro <- indpro %>% mutate(date = as.Date(DATE)) %>% select(date, INDPRO)

# Merge datasets
data <- gs10 %>%
  left_join(gs2, by = "date") %>%
  left_join(recession, by = "date") %>%
  left_join(unrate, by = "date") %>%
  left_join(indpro, by = "date")

# Filter data from 1976 onwards
data <- data %>% filter(date >= as.Date("1976-06-01"))

# Calculate the yield spread
data <- data %>%
  mutate(spread = GS10 - GS2)

# Handling missing values
data <- data %>%
  na.locf()

# Fit the probit model
probit_model <- glm(USRECD ~ spread + UNRATE + INDPRO, family = binomial(link = "probit"), data = data)

# Summary of the model
summary(probit_model)

# NUMBER 6!

# Load additional libraries for model evaluation
install.packages("pscl")
library(pscl)
install.packages("pROC")
library(pROC)

# In-sample fit (pseudo R-squared)
pR2(probit_model)

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fit the model on training data
probit_model_train <- glm(USRECD ~ spread + UNRATE + INDPRO, family = binomial(link = "probit"), data = train_data)

# Predict on test data
test_data <- test_data %>%
  mutate(predicted_prob = predict(probit_model_train, newdata = test_data, type = "response"))

# ROC curve and AUC
roc_curve <- roc(test_data$USRECD, test_data$predicted_prob)
auc(roc_curve)
plot(roc_curve)

# Plotting the spread
plot(data$date, data$spread, type = 'l', col = 'blue', xlab = 'Date', ylab = 'Yield Curve Spread')

# Adding a red horizontal line at y = 0 (zero spread)
abline(h = 0, col = 'red', lwd = 2)

































# VAR APPROACH!

# Convert variables to time series objects
data_ts <- ts(data[, -1], frequency = 12)  # Exclude date column for time series analysis

cor(data_ts)

install.packages("vars")
library(vars)

# Example: VAR with lag order 1 (adjust as needed)
var_model <- VAR(data_ts, p = 1)

summary(var_model)

# Example: Forecasting 12 steps ahead
var_forecast <- predict(var_model, n.ahead = 12)

# Summary of VAR model
var_summary <- summary(var_model)
var_summary

# Example: Forecasting 12 steps ahead
var_forecast <- predict(var_model, n.ahead = 12)
var_forecast




































# SOMETHING ELSE???
# Fit the probit model
probit_model <- glm(USRECD ~ spread + UNRATE + INDPRO, family = binomial(link = "probit"), data = data)

# Load necessary libraries for model evaluation
library(pROC)

# Predict probabilities on the test set
test_data <- test_data %>%
  mutate(predicted_prob = predict(probit_model, newdata = test_data, type = "response"))

# ROC curve and AUC
roc_curve <- roc(test_data$USRECD, test_data$predicted_prob)
auc_value <- auc(roc_curve)
plot(roc_curve, main = "ROC Curve for Recession Prediction")


# SUMMARY 

summary(probit_model)

pR2(probit_model)

# Assuming you've already plotted the ROC curve and calculated AUC
roc_curve <- roc(test_data$USRECD, test_data$predicted_prob)
auc_value <- auc(roc_curve)

# Print AUC value
auc_value




