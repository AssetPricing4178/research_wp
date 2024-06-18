# Load necessary libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(pscl)
library(pROC)
library(fredr)
library(knitr)
library(ggplot2)

# Define the observation start and end dates
start_date <- as.Date("1990-02-01")
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

# Handling missing values
data <- data %>%
  na.locf()

# Define recession 1 year ahead
data <- data %>%
  mutate(recession_lead = lead(USRECD, 12))

# Define recession 2 years ahead
data <- data %>%
  mutate(recession_lead_2y = lead(USRECD, 24))

# Define recession 3 years ahead
data <- data %>%
  mutate(recession_lead_3y = lead(USRECD, 36))

# Remove rows with NA in recession_lead, recession_lead_2y, and recession_lead_3y
data <- data %>% filter(!is.na(recession_lead), !is.na(recession_lead_2y), !is.na(recession_lead_3y))

# Model specifications for 1-year ahead prediction
model_A <- glm(recession_lead ~ spread, data = data, family = binomial(link = "probit"))
model_B <- glm(recession_lead ~ spread + FEDFUNDS, data = data, family = binomial(link = "probit"))
model_C <- glm(recession_lead ~ spread + FEDFUNDS + real_fedfunds, data = data, family = binomial(link = "probit"))
model_D <- glm(recession_lead ~ spread + FEDFUNDS + TERM_PREMIUM, data = data, family = binomial(link = "probit"))

# Model specifications for 2-years ahead prediction
model_A_2y <- glm(recession_lead_2y ~ spread, data = data, family = binomial(link = "probit"))
model_B_2y <- glm(recession_lead_2y ~ spread + FEDFUNDS, data = data, family = binomial(link = "probit"))
model_C_2y <- glm(recession_lead_2y ~ spread + FEDFUNDS + real_fedfunds, data = data, family = binomial(link = "probit"))
model_D_2y <- glm(recession_lead_2y ~ spread + FEDFUNDS + TERM_PREMIUM, data = data, family = binomial(link = "probit"))

# Model specifications for 3-years ahead prediction
model_A_3y <- glm(recession_lead_3y ~ spread, data = data, family = binomial(link = "probit"))
model_B_3y <- glm(recession_lead_3y ~ spread + FEDFUNDS, data = data, family = binomial(link = "probit"))
model_C_3y <- glm(recession_lead_3y ~ spread + FEDFUNDS + real_fedfunds, data = data, family = binomial(link = "probit"))
model_D_3y <- glm(recession_lead_3y ~ spread + FEDFUNDS + TERM_PREMIUM, data = data, family = binomial(link = "probit"))

# Summarize models for 1-year ahead prediction
summary(model_A)
summary(model_B)
summary(model_C)
summary(model_D)

# Summarize models for 2-years ahead prediction
summary(model_A_2y)
summary(model_B_2y)
summary(model_C_2y)
summary(model_D_2y)

# Summarize models for 3-years ahead prediction
summary(model_A_3y)
summary(model_B_3y)
summary(model_C_3y)
summary(model_D_3y)

# Predict probabilities for model D (1-year ahead)
data$predicted_prob <- predict(model_D, type = "response")

# Predict probabilities for model D (2-years ahead)
data$predicted_prob_2y <- predict(model_D_2y, type = "response")

# Predict probabilities for model D (3-years ahead)
data$predicted_prob_3y <- predict(model_D_3y, type = "response")

# Check if predicted_prob, predicted_prob_2y, and predicted_prob_3y columns are created correctly
print(head(data[c("predicted_prob", "predicted_prob_2y", "predicted_prob_3y")]))

# Plot ROC curve for model D (1-year ahead)
roc_curve <- roc(data$recession_lead, data$predicted_prob)
plot(roc_curve)
print(auc(roc_curve))

# Plot ROC curve for model D (2-years ahead)
roc_curve_2y <- roc(data$recession_lead_2y, data$predicted_prob_2y)
plot(roc_curve_2y)
print(auc(roc_curve_2y))

# Plot ROC curve for model D (3-years ahead)
roc_curve_3y <- roc(data$recession_lead_3y, data$predicted_prob_3y)
plot(roc_curve_3y)
print(auc(roc_curve_3y))

# Save the model outputs for 1-year ahead prediction
saveRDS(model_A, "model_A.rds")
saveRDS(model_B, "model_B.rds")
saveRDS(model_C, "model_C.rds")
saveRDS(model_D, "model_D.rds")

# Save the model outputs for 2-years ahead prediction
saveRDS(model_A_2y, "model_A_2y.rds")
saveRDS(model_B_2y, "model_B_2y.rds")
saveRDS(model_C_2y, "model_C_2y.rds")
saveRDS(model_D_2y, "model_D_2y.rds")

# Save the model outputs for 3-years ahead prediction
saveRDS(model_A_3y, "model_A_3y.rds")
saveRDS(model_B_3y, "model_B_3y.rds")
saveRDS(model_C_3y, "model_C_3y.rds")
saveRDS(model_D_3y, "model_D_3y.rds")

# Assuming you have already estimated and stored your models (model_A, model_B, model_C, model_D)




