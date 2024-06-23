# Fit the models
model_a <- glm(USRECD ~ spread, family = binomial(link = "probit"), data = data)
model_b <- glm(USRECD ~ spread + FEDFUNDS, family = binomial(link = "probit"), data = data)

# Display summaries of the models
summary(model_a)
summary(model_b)

# Load necessary libraries
library(pscl)

# Calculate McFadden R-squared
mcfadden_r2 <- function(model, data) {
  null_model <- glm(USRECD ~ 1, family = binomial(link = "probit"), data = data)
  1 - (logLik(model) / logLik(null_model))
}

# Calculate BIC for each model
bic_model_a <- BIC(model_a)
bic_model_b <- BIC(model_b)

# Print comparison
print(mcfadden_r2(model_a, data))
print(bic_model_a)

print(mcfadden_r2(model_b, data))
print(bic_model_b)

# Function to compute pseudo-out-of-sample predictions
pseudo_out_of_sample <- function(model, data, start_index) {
  n <- nrow(data)
  predictions <- numeric(n - start_index)
  actuals <- data$USRECD[(start_index + 1):n]
  
  for (i in start_index:(n - 1)) {
    fit <- update(model, data = data[1:i, ])
    predictions[i - start_index + 1] <- predict(fit, newdata = data[i + 1, ], type = "response")
  }
  
  # Compute RMSE
  rmse <- sqrt(mean((predictions - actuals)^2))
  return(rmse)
}

# Example usage with Model A and Model B
start_index <- 80 # Assuming we start our out-of-sample validation from the 80th observation
rmse_a <- pseudo_out_of_sample(model_a, data, start_index)
rmse_b <- pseudo_out_of_sample(model_b, data, start_index)

# Print RMSEs
print(rmse_a)
print(rmse_b)

# Load necessary libraries for structural change tests
library(strucchange)

# Perform structural change tests
nyblom_test_a <- sctest(efp(USRECD ~ spread, family = binomial(link = "probit"), data = data), type = "Nyblom-Hansen")
sup_lm_test_a <- sctest(efp(USRECD ~ spread, family = binomial(link = "probit"), data = data), type = "supLM")

nyblom_test_b <- sctest(efp(USRECD ~ spread + FEDFUNDS, family = binomial(link = "probit"), data = data), type = "Nyblom-Hansen")
sup_lm_test_b <- sctest(efp(USRECD ~ spread + FEDFUNDS, family = binomial(link = "probit"), data = data), type = "supLM")

# Print test results
print(nyblom_test_a)
print(sup_lm_test_a)

print(nyblom_test_b)
print(sup_lm_test_b)
