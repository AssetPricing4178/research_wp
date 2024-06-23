# Load necessary libraries
library(fredr)
library(dplyr)
library(tidyverse)

# FREDr Key
fredr_set_key("dfdf7dff8e1f85865d60e113b4f67426")
api_key <- fredr_get_key()

# Define observation start and end dates
start_date <- as.Date("1976-06-01")
end_date <- Sys.Date()

# Retrieve data from FRED
gs10 <- fredr(series_id = "GS10", 
              observation_start = start_date, 
              observation_end = end_date,
              frequency = "m") %>%
  select(date, value) %>%
  rename(GS10 = value)

gs2 <- fredr(series_id = "GS2", 
             observation_start = start_date, 
             observation_end = end_date,
             frequency = "m") %>%
  select(date, value) %>%
  rename(GS2 = value)

tb3ms <- fredr(series_id = "TB3MS", 
               observation_start = start_date, 
               observation_end = end_date,
               frequency = "m") %>%
  select(date, value) %>%
  rename(TB3MS = value)

t10y2y <- fredr(series_id = "T10Y2Y", 
                observation_start = start_date, 
                observation_end = end_date,
                frequency = "m") %>%
  select(date, value) %>%
  rename(T10Y2Y = value)

# Merge datasets
economic_data <- gs10 %>%
  left_join(gs2, by = "date") %>%
  left_join(tb3ms, by = "date") %>%
  left_join(t10y2y, by = "date")

# Remove NA values
economic_data <- na.omit(economic_data)

# Calculate spreads and add to economic_data
economic_data <- economic_data %>%
  mutate(spread_10y_2y = GS10 - GS2,
         spread_10y_3m = GS10 - TB3MS)

# Print first few rows of the merged dataset
head(economic_data)

# Plotting T10Y2Y
economic_data %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = T10Y2Y), color = "blue", linetype = "solid") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +
  labs(title = "T10Y2Y with Horizontal Line at 0",
       y = "T10Y2Y",
       x = "Date") +
  theme_minimal()

# Plotting spread between GS10 and GS2
economic_data %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = spread_10y_2y), color = "purple", linetype = "solid") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +
  labs(title = "Spread between GS10 and GS2 with Horizontal Line at 0",
       y = "Spread (GS10 - GS2)",
       x = "Date") +
  theme_minimal()

# Plotting spread between GS10 and TB3MS
economic_data %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = spread_10y_3m), color = "orange", linetype = "solid") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +
  labs(title = "Spread between GS10 and TB3MS with Horizontal Line at 0",
       y = "Spread (GS10 - TB3MS)",
       x = "Date") +
  theme_minimal()

# LAG


