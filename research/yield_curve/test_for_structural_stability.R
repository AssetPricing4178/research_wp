# 1. Split Data by Time Periods

# Split data into two periods
data_pre_2008 <- data[data$date < as.Date("2008-01-01"), ]
data_post_2008 <- data[data$date >= as.Date("2008-01-01"), ]

# 2. Estimate Models for Each Period

# Fit models for period before 2008
model_A_pre <- glm(recession_lead ~ spread, data = data_pre_2008, family = binomial(link = "probit"))
model_B_pre <- glm(recession_lead ~ spread + FEDFUNDS, data = data_pre_2008, family = binomial(link = "probit"))
model_C_pre <- glm(recession_lead ~ spread + FEDFUNDS + real_fedfunds, data = data_pre_2008, family = binomial(link = "probit"))
model_D_pre <- glm(recession_lead ~ spread + FEDFUNDS + TERM_PREMIUM, data = data_pre_2008, family = binomial(link = "probit"))

# Fit models for period after 2008
model_A_post <- glm(recession_lead ~ spread, data = data_post_2008, family = binomial(link = "probit"))
model_B_post <- glm(recession_lead ~ spread + FEDFUNDS, data = data_post_2008, family = binomial(link = "probit"))
model_C_post <- glm(recession_lead ~ spread + FEDFUNDS + real_fedfunds, data = data_post_2008, family = binomial(link = "probit"))
model_D_post <- glm(recession_lead ~ spread + FEDFUNDS + TERM_PREMIUM, data = data_post_2008, family = binomial(link = "probit"))

# 3. Compare Coefficients

# Function to perform Chow Test
chow_test <- function(model_pre, model_post, data_pre, data_post) {
  # Extract coefficients
  coef_pre <- coef(model_pre)
  coef_post <- coef(model_post)
  
  # Concatenate coefficients into a data frame
  coef_data <- data.frame(Variable = rownames(coef_pre),
                          Coefficient_Pre_2008 = coef_pre,
                          Coefficient_Post_2008 = coef_post)
  
  # Perform Chow Test for structural stability
  chow_test_stat <- chow.test(y = data_pre$recession_lead,
                              x = data_pre$spread,
                              group = data_pre$date < as.Date("2008-01-01"))
  
  return(list(coef_data = coef_data, chow_test_stat = chow_test_stat))
}

# Perform Chow Test for each model
chow_model_A <- chow_test(model_A_pre, model_A_post, data_pre_2008, data_post_2008)
chow_model_B <- chow_test(model_B_pre, model_B_post, data_pre_2008, data_post_2008)
chow_model_C <- chow_test(model_C_pre, model_C_post, data_pre_2008, data_post_2008)
chow_model_D <- chow_test(model_D_pre, model_D_post, data_pre_2008, data_post_2008)

# Print coefficients and Chow Test results
print("Model A (spread)")
print(chow_model_A$coef_data)
print("Chow Test:")
print(chow_model_A$chow_test_stat)

# Repeat for other models (model_B, model_C, model_D)


