# 1. SPLIT DATA INTO TRAINING AND TESTING SETS

# Assuming 'data' is already loaded from the prepared_data.rds file
data <- readRDS("prepared_data.rds")

# Set seed for reproducibility
set.seed(123)

# Define the proportion of data to be used for training (e.g., 70% for training, 30% for validation)
train_prop <- 0.7
n <- nrow(data)
train_size <- floor(train_prop * n)

# Create indices for training set
train_indices <- sample(1:n, train_size, replace = FALSE)

# Create training and validation datasets
train_data <- data[train_indices, ]
validation_data <- data[-train_indices, ]

# Optional: Check the dimensions of the training and validation sets
dim(train_data)
dim(validation_data)

# Model specifications for 1-year ahead prediction
model_A <- glm(recession_lead ~ spread, data = train_data, family = binomial(link = "probit"))
model_B <- glm(recession_lead ~ spread + FEDFUNDS, data = train_data, family = binomial(link = "probit"))
model_C <- glm(recession_lead ~ spread + FEDFUNDS + real_fedfunds, data = train_data, family = binomial(link = "probit"))
model_D <- glm(recession_lead ~ spread + FEDFUNDS + TERM_PREMIUM, data = train_data, family = binomial(link = "probit"))

# Model specifications for 2-years ahead prediction
model_A_2y <- glm(recession_lead_2y ~ spread, data = train_data, family = binomial(link = "probit"))
model_B_2y <- glm(recession_lead_2y ~ spread + FEDFUNDS, data = train_data, family = binomial(link = "probit"))
model_C_2y <- glm(recession_lead_2y ~ spread + FEDFUNDS + real_fedfunds, data = train_data, family = binomial(link = "probit"))
model_D_2y <- glm(recession_lead_2y ~ spread + FEDFUNDS + TERM_PREMIUM, data = train_data, family = binomial(link = "probit"))

# Model specifications for 3-years ahead prediction
model_A_3y <- glm(recession_lead_3y ~ spread, data = train_data, family = binomial(link = "probit"))
model_B_3y <- glm(recession_lead_3y ~ spread + FEDFUNDS, data = train_data, family = binomial(link = "probit"))
model_C_3y <- glm(recession_lead_3y ~ spread + FEDFUNDS + real_fedfunds, data = train_data, family = binomial(link = "probit"))
model_D_3y <- glm(recession_lead_3y ~ spread + FEDFUNDS + TERM_PREMIUM, data = train_data, family = binomial(link = "probit"))

# Predict probabilities for models (1-year ahead)
validation_data$predicted_prob_A <- predict(model_A, newdata = validation_data, type = "response")
validation_data$predicted_prob_B <- predict(model_B, newdata = validation_data, type = "response")
validation_data$predicted_prob_C <- predict(model_C, newdata = validation_data, type = "response")
validation_data$predicted_prob_D <- predict(model_D, newdata = validation_data, type = "response")

# Predict probabilities for models (2-years ahead)
validation_data$predicted_prob_A_2y <- predict(model_A_2y, newdata = validation_data, type = "response")
validation_data$predicted_prob_B_2y <- predict(model_B_2y, newdata = validation_data, type = "response")
validation_data$predicted_prob_C_2y <- predict(model_C_2y, newdata = validation_data, type = "response")
validation_data$predicted_prob_D_2y <- predict(model_D_2y, newdata = validation_data, type = "response")

# Predict probabilities for models (3-years ahead)
validation_data$predicted_prob_A_3y <- predict(model_A_3y, newdata = validation_data, type = "response")
validation_data$predicted_prob_B_3y <- predict(model_B_3y, newdata = validation_data, type = "response")
validation_data$predicted_prob_C_3y <- predict(model_C_3y, newdata = validation_data, type = "response")
validation_data$predicted_prob_D_3y <- predict(model_D_3y, newdata = validation_data, type = "response")

# Check if predicted probabilities columns are created correctly
print(head(validation_data[c("predicted_prob_A", "predicted_prob_B", "predicted_prob_C", "predicted_prob_D")]))
print(head(validation_data[c("predicted_prob_A_2y", "predicted_prob_B_2y", "predicted_prob_C_2y", "predicted_prob_D_2y")]))
print(head(validation_data[c("predicted_prob_A_3y", "predicted_prob_B_3y", "predicted_prob_C_3y", "predicted_prob_D_3y")]))

# Plot ROC curves and calculate AUC for models (1-year ahead)
roc_curve_A <- roc(validation_data$recession_lead, validation_data$predicted_prob_A)
roc_curve_B <- roc(validation_data$recession_lead, validation_data$predicted_prob_B)
roc_curve_C <- roc(validation_data$recession_lead, validation_data$predicted_prob_C)
roc_curve_D <- roc(validation_data$recession_lead, validation_data$predicted_prob_D)

plot(roc_curve_A, col = "red")
lines(roc_curve_B, col = "blue")
lines(roc_curve_C, col = "green")
lines(roc_curve_D, col = "purple")
print(auc(roc_curve_A))
print(auc(roc_curve_B))
print(auc(roc_curve_C))
print(auc(roc_curve_D))

# Plot ROC curves and calculate AUC for models (2-years ahead)
roc_curve_A_2y <- roc(validation_data$recession_lead_2y, validation_data$predicted_prob_A_2y)
roc_curve_B_2y <- roc(validation_data$recession_lead_2y, validation_data$predicted_prob_B_2y)
roc_curve_C_2y <- roc(validation_data$recession_lead_2y, validation_data$predicted_prob_C_2y)
roc_curve_D_2y <- roc(validation_data$recession_lead_2y, validation_data$predicted_p

                      