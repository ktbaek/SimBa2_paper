library(tidyverse)
library(magrittr)
library(tidymodels)

b655_s <- read_csv('data/B655_synon.csv') 
s344_s <- read_csv('data/S344_synon.csv') 

# Combine all test sets in one dataframe and nest them into one list-column

test_data <- bind_rows(b655_s, s344_s)

by_test_data <- test_data %>% 
  group_by(dataset) %>% 
  nest() %>% 
  rename(
    test_set = dataset, 
    test_data = data
  ) 


# Predict models

predict_model <- function(cf, data) {
  
  result <-  data %>% 
    mutate(Predicted = pred(RSA, Hdiff, Vdiff, cf))
  
  return(result)
  
}


# Model 1 -----------------------------------------------------------------

# Read model

models <- readRDS("models/models_1.rds") 

# Predict function

pred <- function(RSA, Hdiff, Vdiff, cf) {
  
  cf[1] +
    cf[2] * RSA +
    cf[3] * Hdiff +
    cf[4] * Vdiff +
    cf[5] * RSA * Hdiff +
    cf[6] * RSA * Vdiff
  
}
 
# Combine all combinations of train and test datasets
model_df <- expand_grid(models, by_test_data)

# Predict for each combination of train and test sets
predict_df <- model_df %>% 
  mutate(
    # Predict values
    prediction = map2(coefficients, test_data, ~predict_model(.x, .y))
  )

# Save models and predictions on disk
predict_df %>% 
  saveRDS("models/predict_models_1_synon.rds")


# Model 2 -------------------------------------------------------------

models <- readRDS("models/models_2.rds")

# Combine all combinations of train and test datasets
model_df <- expand_grid(models, by_test_data)

# Predict for each combination of train and test sets
pred <- function(RSA, Hdiff, Vdiff, cf) {
  
  cf[1] * RSA +
    cf[2] * Hdiff +
    cf[3] * Vdiff +
    cf[4] * RSA * Hdiff +
    cf[5] * RSA * Vdiff
  
}

predict_df <- model_df %>% 
  mutate(
    # Predict values
    prediction = map2(coefficients, test_data, ~predict_model(.x, .y))
  )


# Save models and predictions on disk
predict_df %>% 
  saveRDS("models/predict_models_2_synon.rds")

# Models 3 -------------------------------------------------------------

models <- readRDS("models/models_3.rds")

# Combine all combinations of train and test datasets
model_df <- expand_grid(models, by_test_data)

# Predict for each combination of train and test sets
pred <- function(RSA, Hdiff, Vdiff, cf) {
  
  cf[1] * Hdiff +
    cf[2] * Vdiff +
    cf[3] * RSA * Hdiff +
    cf[4] * RSA * Vdiff
  
}

predict_df <- model_df %>% 
  mutate(
    # Predict values
    prediction = map2(coefficients, test_data, ~predict_model(.x, .y))
  )


# Save models and predictions on disk
predict_df %>% 
  saveRDS("models/predict_models_3_synon.rds")
