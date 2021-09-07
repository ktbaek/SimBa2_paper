library(tidyverse)
library(magrittr)
library(tidymodels)
library(krisr)

# Read data sets

b655 <- read_csv('data/B655.csv')
s344 <- read_csv('data/S344.csv')
ssym <- read_csv("data/ssym.csv") 

b1112a <- read_csv('data/B1112a.csv')
b1112b <- read_csv('data/B1112b.csv')
b1112c <- read_csv('data/B1112c.csv')
b1112d <- read_csv('data/B1112d.csv')
b1112e <- read_csv('data/B1112e.csv')

test_data <- bind_rows(
                       b655, 
                       s344, 
                       ssym
                       )

train_data <- bind_rows(
                       b1112a,
                       b1112b,
                       b1112c,
                       b1112d,
                       b1112e
                       )

by_test_data <- test_data %>% 
  group_by(dataset) %>% 
  nest() %>% 
  rename(
    test_set = dataset,
    test_data = data
  ) 

by_train_data <- train_data %>% 
  group_by(dataset) %>% 
  nest() %>% 
  rename(
    test_set = dataset,
    test_data = data
  ) 

# General functions

predict_model <- function(cf, data) {
  
  result <-  data %>% 
    mutate(Predicted = pred(RSA, Hdiff, Vdiff, cf)) %>% 
    rename(
      Experimental = exp_ddG
    ) %>% 
    mutate(
      Residuals = Experimental - Predicted,
      R = sqrt(rsquared(Experimental, Predicted))
    )
  
  return(result)
  
}

prediction_results <- function(df) {
  
  R <- unique(df$R)
  mae <- yardstick::mae(df, truth = Experimental, estimate = Predicted)$.estimate
  mse <- yardstick::msd(df, truth = Experimental, estimate = Predicted)$.estimate  * -1
  rmse <- yardstick::rmse(df, truth = Experimental, estimate = Predicted)$.estimate
  
  lm <- linear_reg() %>% 
    set_engine("lm") %>% 
    fit(Predicted ~ Experimental, data = df) %>%
    tidy()
  
  intercept <- lm %>% filter(term == "(Intercept)") %>% pull(estimate)
  slope <- lm %>% filter(term == "Experimental") %>% pull(estimate)
  
  tibble(
    "R" = R,
    "Slope" = slope,
    "Intercept" = intercept,
    "MAE" = mae,
    "MSE" = mse,
    "RMSE" = rmse
  )
  
}

# Model 1 -----------------------------------------------------------------

models <- readRDS("models/models_1.rds")

# Combine all combinations of train and test datasets
model_df <- expand_grid(models, by_train_data) %>% 
  filter(train_set == test_set) %>% 
  bind_rows(expand_grid(models, by_test_data))

# Predict for each combination of train and test sets
pred <- function(RSA, Hdiff, Vdiff, cf) {
  
  cf[1] +
  cf[2] * RSA +
  cf[3] * Hdiff +
  cf[4] * Vdiff +
  cf[5] * RSA * Hdiff +
  cf[6] * RSA * Vdiff
  
}
predict_df <- model_df %>% 
  mutate(
    # Predict values
    prediction = map2(coefficients, test_data, ~predict_model(.x, .y)),
    # Extract performance metrics
    metrics = map(prediction, ~prediction_results(.x))
  )


# Save models and predictions on disk
predict_df %>% 
  saveRDS("models/predict_models_1.rds")


# Model 2 -------------------------------------------------------------

models <- readRDS("models/models_2.rds")

# Combine all combinations of train and test datasets
model_df <- expand_grid(models, by_train_data) %>% 
  filter(train_set == test_set) %>% 
  bind_rows(expand_grid(models, by_test_data))
  
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
    prediction = map2(coefficients, test_data, ~predict_model(.x, .y)),
    # Extract performance metrics
    metrics = map(prediction, ~prediction_results(.x))
  )

# Save models and predictions on disk
predict_df %>% 
  saveRDS("models/predict_models_2.rds")

# Model 3 -------------------------------------------------------------

models <- readRDS("models/models_3.rds")

# Combine all combinations of train and test datasets
model_df <- expand_grid(models, by_train_data) %>% 
  filter(train_set == test_set) %>% 
  bind_rows(expand_grid(models, by_test_data))

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
    prediction = map2(coefficients, test_data, ~predict_model(.x, .y)),
    # Extract performance metrics
    metrics = map(prediction, ~prediction_results(.x))
  )


# Save models and predictions on disk
predict_df %>% 
  saveRDS("models/predict_models_3.rds")
