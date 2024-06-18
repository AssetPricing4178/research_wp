library(tidyverse)
library(lubridate)
library(fredr)
library(pscl)  # for probit regression

# Define observation start and end dates
start_date <- as.Date("1976-06-01")
end_date <- Sys.Date()

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
data <- data %>% filter(date >= as.Date("1990-01-01"))

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




# Probit regression with yield curve spread
probit_model_spread <- glm(USRECD ~ spread, family = binomial(link = "probit"), data = data)
summary(probit_model_spread)







# WITH LAG TRYYYYYY

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(pscl)  # For probit regression
library(fredr) # For accessing FRED data
library(zoo)   # For handling time series data

# Set FRED API key
fredr_set_key("your_api_key_here")  # Replace with your actual FRED API key

# Define the observation start and end dates
start_date <- as.Date("1976-06-01")
end_date <- Sys.Date()  # Today's date

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

# Create lagged variables for monthly forecasts
data <- data %>%
  mutate(
    lag1_spread = lag(spread, 1),
    lag2_spread = lag(spread, 2),
    lag1_FEDFUNDS = lag(FEDFUNDS, 1),
    lag2_FEDFUNDS = lag(FEDFUNDS, 2)
    # Add other lagged variables similarly
  )

# Fit Probit regression models for forecasting recession probabilities

# Probit regression for 1 month ahead
probit_model_1 <- glm(USRECD ~ lag1_spread + lag1_FEDFUNDS, 
                      family = binomial(link = "probit"), data = data)
summary(probit_model_1)

# Probit regression for 2 months ahead
probit_model_2 <- glm(USRECD ~ lag2_spread + lag2_FEDFUNDS, 
                      family = binomial(link = "probit"), data = data)
summary(probit_model_2)

# Repeat for other forecast horizons (3 months, 4 months, etc.)



# Extract coefficients and their standard errors
coef_summary <- summary(probit_model_1)
beta0 <- coef_summary$coefficients[1, "Estimate"]
beta1 <- coef_summary$coefficients[2, "Estimate"]
se_beta1 <- coef_summary$coefficients[2, "Std. Error"]

# Define range of spread values
spread_values <- seq(-3, 3, by = 0.1)  # Adjust based on your data range

# Calculate linear predictor and probit probabilities
linear_predictor <- beta0 + beta1 * spread_values
probit_probabilities <- pnorm(linear_predictor)

# Calculate upper and lower bounds for specific probabilities
lower_bound <- pnorm(beta0 + beta1 * spread_values + qnorm(0.10) * se_beta1)
middle_bound <- pnorm(beta0 + beta1 * spread_values + qnorm(0.50) * se_beta1)
upper_bound <- pnorm(beta0 + beta1 * spread_values + qnorm(0.90) * se_beta1)

# Plotting
plot(spread_values, probit_probabilities, type = "l",
     xlab = "Lagged Spread (lag1_spread)",
     ylab = "Estimated Probability of Recession",
     main = "Estimated Recession Probabilities",
     ylim = c(0, 1))

# Add lines for specific probabilities
lines(spread_values, lower_bound, col = "blue")
lines(spread_values, middle_bound, col = "green")
lines(spread_values, upper_bound, col = "red")

# Add legend
legend("topright", legend = c("10%", "50%", "90%"), col = c("blue", "green", "red"), lty = 1, cex = 0.8)







# TABLE

# Assuming you have already estimated probit_model_1 and extracted necessary coefficients

# Define spread values range
spread_values <- seq(-3, 3, by = 0.1)  # Adjust based on your data range

# Calculate linear predictor and probit probabilities
linear_predictor <- beta0 + beta1 * spread_values
probit_probabilities <- pnorm(linear_predictor)

# Calculate upper and lower bounds for specific probabilities
quantiles <- c(0.10, 0.50, 0.90)
quantile_names <- c("10%", "50%", "90%")
quantile_bounds <- matrix(NA, nrow = length(spread_values), ncol = length(quantiles))
colnames(quantile_bounds) <- quantile_names

for (i in 1:length(quantiles)) {
  quantile_bounds[, i] <- pnorm(beta0 + beta1 * spread_values + qnorm(quantiles[i]) * se_beta1)
}

# Create a data frame for the table
table_data <- data.frame(spread_values, quantile_bounds)

# Plotting
plot(spread_values, probit_probabilities, type = "l",
     xlab = "Lagged Spread (lag1_spread)",
     ylab = "Estimated Probability of Recession",
     main = "Estimated Recession Probabilities",
     ylim = c(0, 1))

# Add lines for specific probabilities
colors <- c("blue", "green", "red")
for (i in 1:length(quantiles)) {
  lines(spread_values, quantile_bounds[, i], col = colors[i])
}

# Add legend
legend("topright", legend = quantile_names, col = colors, lty = 1, cex = 0.8)

# Print table
cat("Estimated Recession Probabilities for Quantiles of Lagged Spread:\n")
print(table_data)



















