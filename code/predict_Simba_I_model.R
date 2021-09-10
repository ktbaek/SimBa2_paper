library(tidyverse)
library(magrittr)
library(tidymodels)

# Read data sets

b655 <- read_csv('data/B655.csv')
s344 <- read_csv('data/S344.csv')
ssym <- read_csv("data/ssym.csv") 


# General functions

rsquared <- 
  function(x,y) 
  {
    m <- lm(y ~ x)
    return(summary(m)$r.squared)
  }

pred <- function(RSA, Hdiff, Vdiff, cf) {
  
  cf[1] +
    cf[2] * RSA +
    cf[3] * Hdiff +
    cf[4] * Vdiff +
    cf[5] * RSA * Hdiff +
    cf[6] * RSA * Vdiff
  
}

predict_model2 <- function(cf, data) {
  
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
  mae <- mae(df, truth = Experimental, estimate = Predicted)$.estimate
  mse <- msd(df, truth = Experimental, estimate = Predicted)$.estimate * -1
  
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
    "MSE" = mse
  )
  
}


# Combine all test sets in one dataframe and nest them into one list-column

test_data <- bind_rows(b655, s344, ssym)

by_test_data <- test_data %>% 
  group_by(dataset) %>% 
  nest() %>% 
  rename(
    test_set = dataset,
    test_data = data
  ) 

coef <- c(-1.60, 2.20, -0.29, 0.59, 0.49, -0.54)

model_df <- by_test_data %>% 
  mutate(coefficients = list(coef))

# Predict for each combination of train and test sets
predict_df <- model_df %>% 
  mutate(
    # Predict values
    prediction = map2(coefficients, test_data, ~predict_model2(.x, .y)),
    # Extract performance metrics
    metrics = map(prediction, ~prediction_results(.x))
  )


# Save models and predictions on disk
predict_df %>% 
  saveRDS("models/predict_simbaI.rds")
