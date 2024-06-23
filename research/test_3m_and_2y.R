# Create a vector of package names
packages <- c("tidyverse", "lubridate", "zoo", "pscl", "pROC", "fredr", "knitr", "ggplot2")

# Function to check if a package is installed and install it if not
install_if_needed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}


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
end_date <- as.Date("2024-04-01")

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

# Calculate the yield spread GS10 - GS2 (spread_2y)
data <- data %>%
  mutate(spread_2y = GS10 - GS2)

# Retrieve 3-month Treasury bill rate (TB3MS)
tb3ms <- fredr(series_id = "TB3MS", observation_start = start_date, observation_end = end_date) %>%
  select(date, value) %>%
  rename(TB3MS = value)

# Calculate spread_3m (GS10 - TB3MS)
data <- data %>%
  left_join(tb3ms, by = "date") %>%
  mutate(spread_3m = GS10 - TB3MS) %>%
  select(-TB3MS)  # Remove TB3MS after calculating spread

# Probit regression for spread_2y
probit_model_2y <- glm(USRECD ~ spread_2y, family = binomial(link = "probit"), data = data)
summary(probit_model_2y)

# Probit regression for spread_3m
probit_model_3m <- glm(USRECD ~ spread_3m, family = binomial(link = "probit"), data = data)
summary(probit_model_3m)

