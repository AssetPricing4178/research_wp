# DATA

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(pscl)
library(pROC)
library(fredr)
library(knitr)
library(ggplot2)

# Set your FRED API key
fredr_set_key("YOUR_API_KEY")

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

term_premium <- fredr(series_id = "THREEFYTP10", observation_start = start_date, observation_end = end_date) %>%
  select(date, value) %>%
  rename(TERM_PREMIUM = value)

# Merge datasets
data <- gs10 %>%
  left_join(gs2, by = "date") %>%
  left_join(recession, by = "date") %>%
  left_join(fedfunds, by = "date") %>%
  left_join(cpi, by = "date") %>%
  left_join(term_premium, by = "date")

# Filter data from 1990 onwards
data <- data %>% filter(date >= as.Date("1990-02-01"))

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

# Define recession variables 1, 2, and 3 years ahead
data <- data %>%
  mutate(recession_lead = lead(USRECD, 12),
         recession_lead_2y = lead(USRECD, 24),
         recession_lead_3y = lead(USRECD, 36))

# Remove rows with NA in recession_lead variables
data <- data %>% filter(!is.na(recession_lead) & !is.na(recession_lead_2y) & !is.na(recession_lead_3y))

# Save the prepared data to an RDS file for use in the model script
saveRDS(data, "prepared_data.rds")



