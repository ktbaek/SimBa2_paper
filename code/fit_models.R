library(tidyverse)
library(magrittr)
library(tidymodels)

# General functions
rsquared <- 
  function(x,y) 
  {
    m <- lm(y ~ x)
    return(summary(m)$r.squared)
  }

# Training data sets
b1112a <- read_csv('data/B1112a.csv')
b1112b <- read_csv('data/B1112b.csv')
b1112c <- read_csv('data/B1112c.csv')
b1112d <- read_csv('data/B1112d.csv')
b1112e <- read_csv('data/B1112e.csv')

# Combine all training sets in one dataframe and nest them into one list-column
train_data <- bind_rows(b1112a, b1112b, b1112c, b1112d, b1112e)

by_train_data <- train_data %>% 
  group_by(dataset) %>% 
  nest() %>% 
  rename(
    train_set = dataset,
    train_data = data
  ) 

# Model 1 --------------------------------------------

# Fitting function
fit_model <- function(train_data){
  
  optim(
    par = rep(0, 6), #initial guesses
    control = list(trace = FALSE),
    fn = min_fn, 
    data = train_data, 
    method = "BFGS")
  
}

# Fit using RMSE
min_fn <- function(par, data) {
  
  data %>% 
    mutate(prediction = 
             par[1] + 
             par[2] * RSA + 
             par[3] * Hdiff + 
             par[4] * Vdiff +
             par[5] * RSA * Hdiff +
             par[6] * RSA * Vdiff) %>% 
    yardstick::rmse(truth = exp_ddG, estimate = prediction) %>% 
    pull(.estimate)
  
}

model_rmse_df <- 
  by_train_data %>% 
  mutate(model = map(train_data, fit_model),
         method = "RMSE"
  )

# Fit using Huber
min_fn <- function(par, data) {
  
  data %>% 
    mutate(prediction = 
             par[1] + 
             par[2] * RSA + 
             par[3] * Hdiff + 
             par[4] * Vdiff +
             par[5] * RSA * Hdiff +
             par[6] * RSA * Vdiff) %>% 
    yardstick::huber_loss(truth = exp_ddG, estimate = prediction) %>% 
    pull(.estimate)
  
}

model_huber_df <- 
  by_train_data %>% 
  mutate(
    model = map(train_data, fit_model),
    method = "Huber"
  )

# Fit using MAE
min_fn <- function(par, data) {
  
  data %>% 
    mutate(prediction = 
             par[1] + 
             par[2] * RSA + 
             par[3] * Hdiff + 
             par[4] * Vdiff +
             par[5] * RSA * Hdiff +
             par[6] * RSA * Vdiff) %>% 
    yardstick::mae(truth = exp_ddG, estimate = prediction) %>% 
    pull(.estimate)
  
}

model_mae_df <- 
  by_train_data %>% 
  mutate(model = map(train_data, fit_model),
         method = "MAE"
  )



# Combine and save
model_df <- bind_rows(model_rmse_df, model_huber_df, model_mae_df)

model_df %>% 
  mutate(
    coefficients = map(model, ~.x$par),
    features = "model_1"
    ) %>% 
  saveRDS("models/models_1.rds")

# Model 2 -----------------------------------------------------------------

fit_model <- function(train_data){
  
  optim(
    par = rep(0, 5), #initial guesses
    control = list(trace = FALSE),
    fn = min_fn, 
    data = train_data, 
    method = "BFGS")
  
}

# RMSE

min_fn <- function(par, data) {
  
  data %>% 
    mutate(prediction =  
             par[1] * RSA + 
             par[2] * Hdiff + 
             par[3] * Vdiff +
             par[4] * RSA * Hdiff +
             par[5] * RSA * Vdiff) %>% 
    yardstick::rmse(truth = exp_ddG, estimate = prediction) %>% 
    pull(.estimate)
  
}

model_rmse_df <- by_train_data %>% 
  mutate(
    model = map(train_data, fit_model),
    coefficients = map(model, ~.x$par),
    method = "RMSE"
  ) 

# Huber

min_fn <- function(par, data) {
  
  data %>% 
    mutate(prediction =  
             par[1] * RSA + 
             par[2] * Hdiff + 
             par[3] * Vdiff +
             par[4] * RSA * Hdiff +
             par[5] * RSA * Vdiff) %>% 
    yardstick::huber_loss(truth = exp_ddG, estimate = prediction) %>% 
    pull(.estimate)
  
}

model_huber_df <- by_train_data %>% 
  mutate(
    model = map(train_data, fit_model),
    coefficients = map(model, ~.x$par),
    method = "Huber"
  ) 

# MAE

min_fn <- function(par, data) {
  
  data %>% 
    mutate(prediction =  
             par[1] * RSA + 
             par[2] * Hdiff + 
             par[3] * Vdiff +
             par[4] * RSA * Hdiff +
             par[5] * RSA * Vdiff) %>% 
    yardstick::mae(truth = exp_ddG, estimate = prediction) %>% 
    pull(.estimate)
  
}

model_mae_df <- by_train_data %>% 
  mutate(
    model = map(train_data, fit_model),
    coefficients = map(model, ~.x$par),
    method = "MAE"
  ) 

# Combine and save

model_df <- bind_rows(model_rmse_df, model_huber_df, model_mae_df)

model_df %>% 
  mutate(
    features = "model_2"
  ) %>% 
  saveRDS("models/models_2.rds")

# Model 3 -----------------------------------------------------------------

fit_model <- function(train_data){
  
  optim(
    par = rep(0, 4), #initial guesses
    control = list(trace = FALSE),
    fn = min_fn, 
    data = train_data, 
    method = "BFGS")
  
}

# RMSE

min_fn <- function(par, data) {
  
  data %>% 
    mutate(prediction =  
             par[1] * Hdiff + 
             par[2] * Vdiff +
             par[3] * RSA * Hdiff +
             par[4] * RSA * Vdiff) %>% 
    yardstick::rmse(truth = exp_ddG, estimate = prediction) %>% 
    pull(.estimate)
  
}

model_rmse_df <- by_train_data %>% 
  mutate(
    model = map(train_data, fit_model),
    coefficients = map(model, ~.x$par),
    method = "RMSE"
  ) 

# Huber

min_fn <- function(par, data) {
  
  data %>% 
    mutate(prediction =  
             par[1] * Hdiff + 
             par[2] * Vdiff +
             par[3] * RSA * Hdiff +
             par[4] * RSA * Vdiff) %>% 
    yardstick::huber_loss(truth = exp_ddG, estimate = prediction) %>% 
    pull(.estimate)
  
}

model_huber_df <- by_train_data %>% 
  mutate(
    model = map(train_data, fit_model),
    coefficients = map(model, ~.x$par),
    method = "Huber"
  ) 

# MAE

min_fn <- function(par, data) {
  
  data %>% 
    mutate(prediction =  
             par[1] * Hdiff + 
             par[2] * Vdiff +
             par[3] * RSA * Hdiff +
             par[4] * RSA * Vdiff) %>% 
    yardstick::mae(truth = exp_ddG, estimate = prediction) %>% 
    pull(.estimate)
  
}


model_mae_df <- by_train_data %>% 
  mutate(
    model = map(train_data, fit_model),
    coefficients = map(model, ~.x$par),
    method = "MAE"
  ) 

# Combine and save

model_df <- bind_rows(model_rmse_df, model_huber_df, model_mae_df)

model_df %>% 
  mutate(
    features = "model_3"
  ) %>% 
  saveRDS("models/models_3.rds")
