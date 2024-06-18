# Model A

# Load necessary libraries
library(tidyverse)
library(pscl)
library(pROC)

# Load the prepared data
data <- readRDS("prepared_data.rds")

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

# Predict probabilities for models (1-year ahead)
data$predicted_prob_A <- predict(model_A, type = "response")
data$predicted_prob_B <- predict(model_B, type = "response")
data$predicted_prob_C <- predict(model_C, type = "response")
data$predicted_prob_D <- predict(model_D, type = "response")

# Predict probabilities for models (2-years ahead)
data$predicted_prob_A_2y <- predict(model_A_2y, type = "response")
data$predicted_prob_B_2y <- predict(model_B_2y, type = "response")
data$predicted_prob_C_2y <- predict(model_C_2y, type = "response")
data$predicted_prob_D_2y <- predict(model_D_2y, type = "response")

# Predict probabilities for models (3-years ahead)
data$predicted_prob_A_3y <- predict(model_A_3y, type = "response")
data$predicted_prob_B_3y <- predict(model_B_3y, type = "response")
data$predicted_prob_C_3y <- predict(model_C_3y, type = "response")
data$predicted_prob_D_3y <- predict(model_D_3y, type = "response")

# Check if predicted probabilities columns are created correctly
print(head(data[c("predicted_prob_A", "predicted_prob_B", "predicted_prob_C", "predicted_prob_D")]))
print(head(data[c("predicted_prob_A_2y", "predicted_prob_B_2y", "predicted_prob_C_2y", "predicted_prob_D_2y")]))
print(head(data[c("predicted_prob_A_3y", "predicted_prob_B_3y", "predicted_prob_C_3y", "predicted_prob_D_3y")]))

# Plot ROC curves and calculate AUC for models (1-year ahead)
roc_curve_A <- roc(data$recession_lead, data$predicted_prob_A)
roc_curve_B <- roc(data$recession_lead, data$predicted_prob_B)
roc_curve_C <- roc(data$recession_lead, data$predicted_prob_C)
roc_curve_D <- roc(data$recession_lead, data$predicted_prob_D)

plot(roc_curve_A, col = "red")
lines(roc_curve_B, col = "blue")
lines(roc_curve_C, col = "green")
lines(roc_curve_D, col = "purple")
print(auc(roc_curve_A))
print(auc(roc_curve_B))
print(auc(roc_curve_C))
print(auc(roc_curve_D))

# Plot ROC curves and calculate AUC for models (2-years ahead)
roc_curve_A_2y <- roc(data$recession_lead_2y, data$predicted_prob_A_2y)
roc_curve_B_2y <- roc(data$recession_lead_2y, data$predicted_prob_B_2y)
roc_curve_C_2y <- roc(data$recession_lead_2y, data$predicted_prob_C_2y)
roc_curve_D_2y <- roc(data$recession_lead_2y, data$predicted_prob_D_2y)

plot(roc_curve_A_2y, col = "red")
lines(roc_curve_B_2y, col = "blue")
lines(roc_curve_C_2y, col = "green")
lines(roc_curve_D_2y, col = "purple")
print(auc(roc_curve_A_2y))
print(auc(roc_curve_B_2y))
print(auc(roc_curve_C_2y))
print(auc(roc_curve_D_2y))

# Plot ROC curves and calculate AUC for models (3-years ahead)
roc_curve_A_3y <- roc(data$recession_lead_3y, data$predicted_prob_A_3y)
roc_curve_B_3y <- roc(data$recession_lead_3y, data$predicted_prob_B_3y)
roc_curve_C_3y <- roc(data$recession_lead_3y, data$predicted_prob_C_3y)
roc_curve_D_3y <- roc(data$recession_lead_3y, data$predicted_prob_D_3y)

plot(roc_curve_A_3y, col = "red")
lines(roc_curve_B_3y, col = "blue")
lines(roc_curve_C_3y, col = "green")
lines(roc_curve_D_3y, col = "purple")
print(auc(roc_curve_A_3y))
print(auc(roc_curve_B_3y))
print(auc(roc_curve_C_3y))
print(auc(roc_curve_D_3y))

# Save the model outputs for 1-year ahead prediction
saveRDS(model_A, "model_A_1y.rds")
saveRDS(model_B, "model_B_1y.rds")
saveRDS(model_C, "model_C_1y.rds")
saveRDS(model_D, "model_D_1y.rds")

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

