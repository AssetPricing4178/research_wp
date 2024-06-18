library(pROC)  # For ROC analysis if needed

# Assuming data is already prepared and models (probit_model, probit_model_FF, probit_model_RFF) are defined

# Define the periods for train-test split
train_test_periods <- list(
  list(train_start = as.Date("1976-06-01"), train_end = as.Date("1995-06-01"),
       test_start = as.Date("1995-07-01"), test_end = as.Date("2004-06-01")),
  list(train_start = as.Date("2004-06-01"), train_end = as.Date("2013-06-01"),
       test_start = as.Date("2013-07-01"), test_end = as.Date("2022-06-01")),
  list(train_start = as.Date("1995-06-01"), train_end = as.Date("2013-06-01"),
       test_start = as.Date("2013-07-01"), test_end = as.Date("2024-04-01"))
)


# Function to evaluate model performance (e.g., AUC-ROC)
evaluate_model <- function(model, train_data, test_data) {
  # Train the model
  fit <- glm(USRECD ~ spread + FEDFUNDS + real_fedfunds, family = binomial(link = "probit"), data = train_data)
  
  # Predict probabilities on test data
  prob_pred <- predict(fit, newdata = test_data, type = "response")
  
  # Calculate AUC-ROC
  roc_obj <- roc(test_data$USRECD, prob_pred)
  return(auc(roc_obj))
}

# Initialize vectors to store results
auc_results <- numeric(length(train_test_periods))

# Perform train-test split validation for each period
for (i in seq_along(train_test_periods)) {
  # Extract dates for current period
  train_start <- train_test_periods[[i]]$train_start
  train_end <- train_test_periods[[i]]$train_end
  test_start <- train_test_periods[[i]]$test_start
  test_end <- train_test_periods[[i]]$test_end
  
  # Subset data for current period (monthly data)
  train_data <- data[data$date >= train_start & data$date <= train_end, ]
  test_data <- data[data$date >= test_start & data$date <= test_end, ]
  
  # Evaluate each model on the test data
  auc_results[i] <- evaluate_model(probit_model_RFF, train_data, test_data)
  
  # Print results for the current period
  cat(sprintf("AUC-ROC for Period %s to %s: %.4f\n", format(train_start, "%Y-%m"), format(test_end, "%Y-%m"), auc_results[i]))
}

# Overall performance summary
cat("\nAverage AUC-ROC across all periods:", mean(auc_results), "\n")
