# Load necessary libraries
library(tidyverse)
library(lubridate)
library(pscl)  # For probit regression
library(fredr)
library(zoo)   # For na.locf

# FREDr Key
fredr_set_key("dfdf7dff8e1f85865d60e113b4f67426")
api_key <- fredr_get_key()

# Define the observation start and end dates
start_date <- as.Date("1976-06-01")
end_date <- as.Date("2024-04-01")

# Retrieve data from FRED
gs10 <- fredr(series_id = "T10Y2Y", observation_start = start_date, observation_end = end_date) %>%
  select(date, value) %>%
  rename(gs10 = value)

recession <- fredr(series_id = "USRECD", observation_start = start_date, observation_end = end_date) %>%
  select(date, value) %>%
  rename(recession = value)

fedfunds <- fredr(series_id = "FEDFUNDS", observation_start = start_date, observation_end = end_date) %>%
  select(date, value) %>%
  rename(fedfunds = value)

cpi <- fredr(series_id = "CPIAUCSL", observation_start = start_date, observation_end = end_date) %>%
  select(date, value) %>%
  rename(cpi = value)

# Merge datasets
data <- gs10 %>%
  left_join(recession, by = "date") %>%
  left_join(fedfunds, by = "date") %>%
  left_join(cpi, by = "date")

# Filter data from 1976 onwards
data <- data %>% filter(date >= as.Date("1976-06-01"))

# Calculate the real federal funds rate
data <- data %>%
  mutate(inflation = (cpi / lag(cpi, 12)) - 1,
         real_fedfunds = fedfunds - (inflation * 100))

# Handle missing values by forward filling
data <- data %>%
  na.locf(na.rm = FALSE)

# Probit regression
probit_model <- glm(recession ~ gs10, family = binomial(link = "probit"), data = data)
summary(probit_model)

# Probit regression with FF
probit_model_FF <- glm(recession ~ gs10 + fedfunds, family = binomial(link = "probit"), data = data)
summary(probit_model_FF)

# Probit regression with FF and RFF
probit_model_RFF <- glm(recession ~ gs10 + fedfunds + real_fedfunds, family = binomial(link = "probit"), data = data)
summary(probit_model_RFF)
