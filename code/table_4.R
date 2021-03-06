library(tidyverse)
library(magrittr)
library(tidymodels)

# Read data sets

ssym <- read_csv("data/ssym.csv") 

# General functions

rsquared <- 
  function(x,y) 
  {
    m <- lm(y ~ x)
    return(summary(m)$r.squared)
  }

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
  rmse <- yardstick::rmse(df, truth = Experimental, estimate = Predicted)$.estimate
  
  lm <- linear_reg() %>% 
    set_engine("lm") %>% 
    fit(Predicted ~ Experimental, data = df) %>%
    tidy()
  
  intercept <- lm %>% filter(term == "(Intercept)") %>% pull(estimate)
  slope <- lm %>% filter(term == "Experimental") %>% pull(estimate)
  
  tibble(
    "R" = R,
    "RMSE" = rmse
  )
  
}

delta_average <- function(df) {
  
  predict_df %>% 
    unnest(prediction) %>% 
    select(Mut_index, Direction, Predicted) %>% 
    pivot_wider(names_from = Direction, values_from = Predicted) %>% 
    mutate(delta = direct + reverse) %>% 
    summarize(delta_avg = mean(delta, na.rm = TRUE)) %>% 
    pull(delta_avg)
  
}

corr_coef <- function(df) {
  
  df %>% 
    select(model, prediction, Direction) %>% 
    unnest(prediction) %>% 
    select(Mut_index, Direction, Predicted) %>% 
    pivot_wider(names_from = Direction, values_from = Predicted) %>% 
    mutate(R = -1 * sqrt(rsquared(direct, reverse))) %>% 
    pull(R) %>% 
    unique()
  
}
# SimBa-SYM -------------------------------------------------------------

models <- readRDS("models/models_3.rds")

test_data <- ssym %>% 
  group_by(Direction) %>% 
  nest() %>% 
  rename(
    test_data = data
  ) 

model <- models %>% 
  filter(
    method == "Huber", 
    train_set == "B1112d"
  ) 

model_df <- expand_grid(model, test_data)

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
  ) %>% 
  mutate(model = "SimBa-SYM") %>% 
  select(-method, -features, -train_set, -train_data)

delta_avg <- delta_average(predict_df)
corr <- corr_coef(predict_df)

# Raw data table

predict_df %>% 
  select(model, Direction, prediction) %>% 
  unnest(prediction) %>% 
  rename(SimBa_SYM_predict = Predicted) %>% 
  write_csv("data/simba_sym_ssym_predict.csv")

# Metrics

simba_sym <- predict_df %>% 
  select(model, Direction, metrics) %>% 
  unnest(metrics) %>% 
  pivot_wider(names_from = Direction, values_from = c(R, RMSE)) %>% 
  mutate(
    delta_avg = delta_avg,
    Rdirinv = corr) %>% 
  mutate(across(where(is.numeric), sprintf, fmt = "%.2f")) %>% 
  select(model, RMSE_direct, R_direct, RMSE_reverse, R_reverse, Rdirinv, delta_avg)

# SimBa-IB -------------------------------------------------------------

models <- readRDS("models/models_1.rds")

test_data <- ssym %>% 
  group_by(Direction) %>% 
  nest() %>% 
  rename(
    test_data = data
  ) 

model <- models %>% 
  filter(
    method == "Huber", 
    train_set == "B1112d"
  ) 

model_df <- expand_grid(model, test_data)

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
  ) %>% 
  mutate(model = "SimBa-IB") %>% 
  select(-method, -features, -train_set, -train_data)

delta_avg <- delta_average(predict_df)
corr <- corr_coef(predict_df)

# Raw data table

predict_df %>% 
  select(model, Direction, prediction) %>% 
  unnest(prediction) %>% 
  rename(SimBa_IB_predict = Predicted) %>% 
  write_csv("data/simba_IB_ssym_predict.csv")

# Metrics

simba_ib <- predict_df %>% 
  select(model, Direction, metrics) %>% 
  unnest(metrics) %>% 
  pivot_wider(names_from = Direction, values_from = c(R, RMSE)) %>% 
  mutate(
    delta_avg = delta_avg,
    Rdirinv = corr) %>% 
  mutate(across(where(is.numeric), sprintf, fmt = "%.2f")) %>% 
  select(model, RMSE_direct, R_direct, RMSE_reverse, R_reverse, Rdirinv, delta_avg)

# SimBa-I -------------------------------------------------------------

coef <- c(-1.60, 2.20, -0.29, 0.59, 0.49, -0.54)

model_df <- test_data %>% 
  mutate(coefficients = list(coef))

test_data <- ssym %>% 
  group_by(Direction) %>% 
  nest() %>% 
  rename(
    test_data = data
  ) 


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
  ) %>% 
  mutate(model = "SimBa-I") 

delta_avg <- delta_average(predict_df)
corr <- corr_coef(predict_df)

simba_i <- predict_df %>% 
  select(model, Direction, metrics) %>% 
  unnest(metrics) %>% 
  pivot_wider(names_from = Direction, values_from = c(R, RMSE)) %>% 
  mutate(
    delta_avg = delta_avg,
    Rdirinv = corr) %>% 
  mutate(across(where(is.numeric), sprintf, fmt = "%.2f")) %>% 
  select(model, RMSE_direct, R_direct, RMSE_reverse, R_reverse, Rdirinv, delta_avg)

bind_rows(simba_i, simba_ib, simba_sym) %>% 
  write.table(file = "tables/table_4.txt", sep = ",", quote = FALSE, row.names = FALSE)




