# Load necessary libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(pscl)
library(pROC)
library(fredr)
library(knitr)
library(ggplot2)

# FREDr Key
fredr_set_key("dfdf7dff8e1f85865d60e113b4f67426")
api_key <- fredr_get_key()

# Define the observation start and end dates
start_date <- as.Date("1976-06-01")
end_date <- as.Date(Sys.Date())

# Retrieve data from FRED
gs10 <- fredr(series_id = "GS10", observation_start = start_date, observation_end = end_date) %>%
  select(date, value) %>%
  rename(GS10 = value)

gs2 <- fredr(series_id = "GS2", observation_start = start_date, observation_end = end_date) %>%
  select(date, value) %>%
  rename(GS2 = value)

recession <- fredr(series_id = "USRECD", observation_start = start_date, observation_end = end_date) %>%
  select(date, value) %>%
  rename(USRECD = value)

fedfunds <- fredr(series_id = "FEDFUNDS", observation_start = start_date, observation_end = end_date) %>%
  select(date, value) %>%
  rename(FEDFUNDS = value)

cpi <- fredr(series_id = "CPIAUCSL", observation_start = start_date, observation_end = end_date) %>%
  select(date, value) %>%
  rename(CPIAUCSL = value)

# Merge datasets
data <- gs10 %>%
  left_join(gs2, by = "date") %>%
  left_join(recession, by = "date") %>%
  left_join(fedfunds, by = "date") %>%
  left_join(cpi, by = "date")

# Filter data from 1990 onwards
data <- data %>% filter(date >= as.Date("1976-06-01"))

# Calculate the yield spread
data <- data %>%
  mutate(spread = GS10 - GS2)

# Calculate the real federal funds rate
data <- data %>%
  mutate(inflation = (CPIAUCSL / lag(CPIAUCSL, 12)) - 1,
         real_fedfunds = FEDFUNDS - (inflation * 100))

# Handle missing values by forward filling
data <- data %>%
  na.locf(na.rm = FALSE)

# Probit regression
probit_model <- glm(USRECD ~ spread, family = binomial(link = "probit"), data = data)
summary(probit_model)

# Probit regression with FF
probit_model_FF <- glm(USRECD ~ spread + FEDFUNDS, family = binomial(link = "probit"), data = data)
summary(probit_model_FF)


# Probit regression with FF and RFF
probit_model_RFF <- glm(USRECD ~ spread + FEDFUNDS + real_fedfunds, family = binomial(link = "probit"), data = data)
summary(probit_model_RFF)

























# NEW SECTION! 

# Probit regression with spread, FEDFUNDS, and real_fedfunds
probit_model_full <- glm(USRECD ~ spread + FEDFUNDS + real_fedfunds, family = binomial(link = "probit"), data = data)

# Summary of the full model
summary(probit_model_full)

# Robustness test: Add lagged spread
data <- data %>%
  mutate(spread_lag1 = lag(spread))

probit_model_lag <- glm(USRECD ~ spread_lag1 + FEDFUNDS + real_fedfunds, family = binomial(link = "probit"), data = data, na.action = na.exclude)

# Summary of the lagged model
summary(probit_model_lag)

# Out-of-sample validation (e.g., rolling window approach)
n <- nrow(data)
window_size <- 120  # Number of observations in each window
results <- data.frame()

for (i in 1:(n - window_size + 1)) {
  train_data <- data[i:(i + window_size - 1), ]
  test_data <- data[(i + window_size):(i + window_size), ]
  
  model <- glm(USRECD ~ spread + FEDFUNDS + real_fedfunds, family = binomial(link = "probit"), data = train_data)
  
  # Predict on test data
  test_data$predicted <- predict(model, newdata = test_data, type = "response")
  
  results <- rbind(results, test_data)
}

# Evaluate model performance (e.g., accuracy, AUC-ROC)
roc_obj <- roc(results$USRECD, results$predicted)
auc <- auc(roc_obj)

accuracy <- sum(results$USRECD == round(results$predicted)) / nrow(results)
sensitivity <- sum(results$USRECD == 1 & round(results$predicted) == 1) / sum(results$USRECD == 1)
specificity <- sum(results$USRECD == 0 & round(results$predicted) == 0) / sum(results$USRECD == 0)

cat("Accuracy:", accuracy, "\n")
cat("AUC-ROC:", auc, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")


















