library(dplyr)
library(zoo)
library(fredr)

# Set FRED API key
fredr_set_key("dfdf7dff8e1f85865d60e113b4f67426")

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

# Filter data from 1976 onwards
data <- data %>% filter(date >= as.Date("1976-06-01"))

# Calculate the yield spread
data <- data %>%
  mutate(spread = GS10 - GS2)

# Calculate the real federal funds rate
data <- data %>%
  mutate(inflation = (CPIAUCSL / lag(CPIAUCSL, 12)) - 1,
         real_fedfunds = FEDFUNDS - (inflation * 100))

# Create lagged variables
data <- data %>%
  mutate(
    lag1_spread = lag(spread, 1),
    lag6_spread = lag(spread, 6),
    lag12_spread = lag(spread, 12),
    lag1_FEDFUNDS = lag(FEDFUNDS, 1),
    lag6_FEDFUNDS = lag(FEDFUNDS, 6),
    lag12_FEDFUNDS = lag(FEDFUNDS, 12),
    lag1_real_fedfunds = lag(real_fedfunds, 1),
    lag6_real_fedfunds = lag(real_fedfunds, 6),
    lag12_real_fedfunds = lag(real_fedfunds, 12)
  )

# Handle missing values by forward filling
data <- data %>%
  na.locf(na.rm = FALSE)

# Probit regression with various lags
probit_model_various_lags <- glm(USRECD ~ lag1_spread + lag6_spread + lag12_spread + lag1_FEDFUNDS + lag6_FEDFUNDS + lag12_FEDFUNDS + lag1_real_fedfunds + lag6_real_fedfunds + lag12_real_fedfunds, family = binomial(link = "probit"), data = data)
summary(probit_model_various_lags)


# Probit regression with lagged spread
probit_model_lagged_spread <- glm(USRECD ~ lag1_spread + lag2_spread, family = binomial(link = "probit"), data = data)
summary(probit_model_lagged_spread)

# Probit regression with lagged spread and FF
probit_model_lagged_FF <- glm(USRECD ~ lag1_spread + lag2_spread + lag1_FEDFUNDS + lag2_FEDFUNDS, family = binomial(link = "probit"), data = data)
summary(probit_model_lagged_FF)

# Probit regression with lagged spread, FF, and real_fedfunds
probit_model_lagged_all <- glm(USRECD ~ lag1_spread + lag2_spread + lag1_FEDFUNDS + lag2_FEDFUNDS + real_fedfunds, family = binomial(link = "probit"), data = data)
summary(probit_model_lagged_all)
