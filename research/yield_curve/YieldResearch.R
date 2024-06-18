# Load necessary libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(pscl)
library(pROC)

# Load the data
gs10 <- read.csv("GS10.csv") %>% mutate(date = as.Date(DATE)) %>% select(date, GS10)
gs2 <- read.csv("GS2.csv") %>% mutate(date = as.Date(DATE)) %>% select(date, GS2)
recession <- read.csv("USRECD.csv") %>% mutate(date = as.Date(DATE)) %>% select(date, USRECD)
unrate <- read.csv("UNRATE.csv") %>% mutate(date = as.Date(DATE)) %>% select(date, UNRATE)
indpro <- read.csv("INDPRO.csv") %>% mutate(date = as.Date(DATE)) %>% select(date, INDPRO)
fedfunds <- read.csv("FEDFUNDS.csv") %>% mutate(date = as.Date(DATE)) %>% select(date, DFF)

# Merge datasets
data <- gs10 %>%
  left_join(gs2, by = "date") %>%
  left_join(recession, by = "date") %>%
  left_join(unrate, by = "date") %>%
  left_join(indpro, by = "date") %>%
  left_join(fedfunds, by = "date")

# Filter data from 1976 onwards
data <- data %>% filter(date >= as.Date("1976-06-01"))

# Calculate the yield spread
data <- data %>%
  mutate(spread = GS10 - GS2)

# Handling missing values
data <- data %>%
  na.locf()

# Fit the initial probit model
probit_model_initial <- glm(USRECD ~ spread + UNRATE + INDPRO + DFF, 
                            family = binomial(link = "probit"), data = data)

# Summary of the initial model
summary(probit_model_initial)

# In-sample fit (pseudo R-squared)
pR2(probit_model_initial)

# Split data into training and testing sets for initial model
set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fit the initial model on training data
probit_model_train_initial <- glm(USRECD ~ spread + UNRATE + INDPRO + DFF, 
                                  family = binomial(link = "probit"), data = train_data)

# Predict on test data for initial model
test_data <- test_data %>%
  mutate(predicted_prob_initial = predict(probit_model_train_initial, 
                                          newdata = test_data, type = "response"))

# ROC curve and AUC for initial model
roc_curve_initial <- roc(test_data$USRECD, test_data$predicted_prob_initial)
auc(roc_curve_initial)
plot(roc_curve_initial)

# Plotting the spread
plot(data$date, data$spread, type = 'l', col = 'blue', xlab = 'Date', ylab = 'Yield Curve Spread')
abline(h = 0, col = 'red', lwd = 2)  # Adding a red horizontal line at y = 0 (zero spread)

#### Dynamic Probit Model with Lagged Variables

# Create lagged variables for UNRATE, INDPRO, and DFF
data <- data %>%
  mutate(UNRATE_lag1 = lag(UNRATE, 1),
         UNRATE_lag2 = lag(UNRATE, 2),
         INDPRO_lag1 = lag(INDPRO, 1),
         INDPRO_lag2 = lag(INDPRO, 2),
         DFF_lag1 = lag(DFF, 1),
         DFF_lag2 = lag(DFF, 2))

# Handling missing values
data <- data %>%
  na.locf()

# Fit the dynamic probit model
probit_model_dynamic <- glm(USRECD ~ spread + UNRATE + UNRATE_lag1 + UNRATE_lag2 + 
                              INDPRO + INDPRO_lag1 + INDPRO_lag2 + 
                              DFF + DFF_lag1 + DFF_lag2,
                            family = binomial(link = "probit"), data = data)

# Summary of the dynamic model
summary(probit_model_dynamic)

# In-sample fit (pseudo R-squared) for dynamic model
pR2(probit_model_dynamic)

# Split data into training and testing sets for dynamic model
set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fit the dynamic model on training data
probit_model_train_dynamic <- glm(USRECD ~ spread + UNRATE + UNRATE_lag1 + UNRATE_lag2 + 
                                    INDPRO + INDPRO_lag1 + INDPRO_lag2 + 
                                    DFF + DFF_lag1 + DFF_lag2,
                                  family = binomial(link = "probit"), data = train_data)

# Predict on test data for dynamic model
test_data <- test_data %>%
  mutate(predicted_prob_dynamic = predict(probit_model_train_dynamic, 
                                          newdata = test_data, type = "response"))

# ROC curve and AUC for dynamic model
roc_curve_dynamic <- roc(test_data$USRECD, test_data$predicted_prob_dynamic)
auc(roc_curve_dynamic)
plot(roc_curve_dynamic)

# Plotting the spread
plot(data$date, data$spread, type = 'l', col = 'blue', xlab = 'Date', ylab = 'Yield Curve Spread')
abline(h = 0, col = 'red', lwd = 2)  # Adding a red horizontal line at y = 0 (zero spread)



































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




