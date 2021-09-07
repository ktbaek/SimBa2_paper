library(tidyverse)
library(magrittr)

# Training data sets
b1112a <- read_csv('data/B1112a.csv')
b1112b <- read_csv('data/B1112b.csv')
b1112c <- read_csv('data/B1112c.csv')
b1112d <- read_csv('data/B1112d.csv')
b1112e <- read_csv('data/B1112e.csv')

# Functions
run_summarize <- function(predict_df, parameters){
  
  # Unnest performance metrics
  summary_df <- predict_df %>% 
    select(method, train_set, test_set, metrics) %>% 
    unnest(cols = c(metrics)) 
  
  # Keep only predictions on the training sets used for training (e.g discard B1112a B1112b combination)
  self_test_df <- summary_df %>% 
    filter(
      str_detect(test_set, "B1112"),
      train_set == test_set
    )
  
  # Make summary table of performance metrics
  summary_df %<>% 
    filter(!str_detect(test_set, "B1112")) %>% 
    bind_rows(self_test_df) %>% 
    arrange(test_set, train_set, method) %>% 
    mutate(across(where(is.numeric), sprintf, fmt = "%.2f")) %>% 
    fix_colnames_metrics() 
  
  # Make summary table of coefficients
  coef <- predict_df %>% 
    select(method, features, train_set, coefficients) %>% 
    distinct() %>% 
    unnest(cols = c(coefficients)) %>% 
    mutate(term = rep(parameters, 15)) %>% 
    pivot_wider(names_from = term, values_from = coefficients) %>% 
    mutate(across(where(is.numeric), sprintf, fmt = "%.3f")) %>% 
    arrange(train_set, method) %>% 
    fix_colnames_coef() 
  
  return(list("models" = coef, "predictions" = summary_df))
  
}

fix_colnames_coef <- function(df) {
  
  df %>% 
    rename(
      "Fitting method" = method,
      "Training dataset" = train_set,
      "RSA x Hdiff" = `RSA:Hdiff`,
      "RSA x Vdiff" = `RSA:Vdiff`
      ) 
  
}

fix_colnames_metrics <- function(df) {
  
  df %>% 
    rename(
      "Fitting method" = method,
      "Training dataset" = train_set,
      "Testing dataset" = test_set
    ) 
  
}

# Model 1 -----------------------------------------------------------------

predict_df <- readRDS("models/predict_models_1.rds") 

predict_df$method <- factor(predict_df$method, levels = c("RMSE", "Huber", "MAE"))

features <- c("Intercept", "RSA", "Hdiff", "Vdiff", "RSA:Hdiff", "RSA:Vdiff")

summary <- run_summarize(predict_df, features)

summary$models %>% 
  write.table(file = "tables/table_S1.txt", sep = ",", quote = FALSE, row.names = FALSE)

summary$predictions %>% 
  filter(!str_detect(`Testing dataset`, "B1112")) %>% 
  write.table(file = "tables/table_S2.txt", sep = ",", quote = FALSE, row.names = FALSE)

summary$predictions %>% 
  filter(str_detect(`Testing dataset`, "B1112")) %>% 
  write.table(file = "tables/table_S2_training_set.txt", sep = ",", quote = FALSE, row.names = FALSE)

# Model 2 -------------------------------------------

predict_df <- readRDS("models/predict_models_2.rds") 
  
predict_df$method <- factor(predict_df$method, levels = c("RMSE", "Huber", "MAE"))

features <- c("RSA", "Hdiff", "Vdiff", "RSA:Hdiff", "RSA:Vdiff")

summary <- run_summarize(predict_df, features)

summary$models %>% 
  write.table(file = "tables/table_S3.txt", sep = ",", quote = FALSE, row.names = FALSE)

summary$predictions %>% 
  filter(!str_detect(`Testing dataset`, "B1112")) %>% 
  write.table(file = "tables/table_S4.txt", sep = ",", quote = FALSE, row.names = FALSE)

summary$predictions %>% 
  filter(str_detect(`Testing dataset`, "B1112")) %>% 
  write.table(file = "tables/table_S4_training_set.txt", sep = ",", quote = FALSE, row.names = FALSE)

# Model 3 -------------------------------------------

predict_df <- readRDS("models/predict_models_3.rds") 

predict_df$method <- factor(predict_df$method, levels = c("RMSE", "Huber", "MAE"))

features <- c( "Hdiff", "Vdiff", "RSA:Hdiff", "RSA:Vdiff")

summary <- run_summarize(predict_df, features)

summary$models %>% 
  write.table(file = "tables/table_S5.txt", sep = ",", quote = FALSE, row.names = FALSE)

summary$predictions %>% 
  filter(!str_detect(`Testing dataset`, "B1112")) %>% 
  write.table(file = "tables/table_S6.txt", sep = ",", quote = FALSE, row.names = FALSE)

summary$predictions %>% 
  filter(str_detect(`Testing dataset`, "B1112")) %>% 
  write.table(file = "tables/table_S6_training_set.txt", sep = ",", quote = FALSE, row.names = FALSE)


# Comparing tables --------------------------------------------------------

predict_simba1 <- readRDS("models/predict_SimbaI.rds") 

# Unnest performance metrics
final_simbaI <- predict_simba1 %>% 
  select(test_set, metrics) %>% 
  unnest(cols = c(metrics)) %>% 
  mutate(model = "Simba_I", .before = test_set)

predict_simba2A <- 
  readRDS("models/predict_models_1.rds") 

summary_simba2A <- predict_simba2A %>% 
  select(method, features, train_set, test_set, metrics) %>% 
  unnest() 

final_simba2A <- summary_simba2A %>% 
  filter(
    method == "Huber", 
    train_set == "B1112d",
    test_set %in% c("B655", "S344", "Ssym")
  ) %>% 
  select(-method, -features, -train_set) %>% 
  mutate(model = "Simba_2A", .before = test_set) 

predict_simba2B <- 
  readRDS("models/predict_models_3.rds") 

summary_simba2B  <- predict_simba2B %>% 
  select(method, features, train_set, test_set, metrics) %>% 
  unnest() 

final_simba2B <- summary_simba2B %>% 
  filter(
    method == "Huber", 
    train_set == "B1112d",
    test_set %in% c("B655", "S344", "Ssym")
  ) %>% 
  select(-method, -train_set) %>% 
  mutate(model = "Simba_2B", .before = test_set) 

bind_rows(final_simbaI, final_simba2A, final_simba2B) %>% 
  mutate(across(where(is.numeric), sprintf, fmt = "%.2f")) %>% 
  write.table(file = "tables/table_3.txt", sep = ",", quote = FALSE, row.names = FALSE)

####

coef_simba1 <- predict_simba1 %>%
  ungroup() %>% 
  select(coefficients) %>% 
  distinct() %>% 
  unnest(cols = c(coefficients)) %>% 
  mutate(term = c("Intercept", "RSA", "Hdiff", "Vdiff", "RSA:Hdiff", "RSA:Vdiff")) %>% 
  pivot_wider(names_from = term, values_from = coefficients) %>% 
  mutate(across(where(is.numeric), sprintf, fmt = "%.3f")) %>% 
  mutate(model = "Simba1", .before = Intercept)

coef_simba2A <- predict_simba2A %>%
  filter(
    method == "Huber", 
    train_set == "B1112d",
    test_set %in% c("B655", "S344", "Ssym")
  ) %>% 
  select(coefficients) %>% 
  distinct() %>% 
  unnest(cols = c(coefficients)) %>% 
  mutate(term = c("Intercept", "RSA", "Hdiff", "Vdiff", "RSA:Hdiff", "RSA:Vdiff")) %>% 
  pivot_wider(names_from = term, values_from = coefficients) %>% 
  mutate(across(where(is.numeric), sprintf, fmt = "%.3f")) %>% 
  mutate(model = "Simba2_A", .before = Intercept)

coef_simba2B <- predict_simba2B %>%
  filter(
    method == "Huber", 
    train_set == "B1112d",
    test_set %in% c("B655", "S344", "Ssym")
  ) %>% 
  select(coefficients) %>% 
  distinct() %>% 
  unnest(cols = c(coefficients)) %>% 
  mutate(term = c("Hdiff", "Vdiff", "RSA:Hdiff", "RSA:Vdiff")) %>% 
  pivot_wider(names_from = term, values_from = coefficients) %>% 
  mutate(across(where(is.numeric), sprintf, fmt = "%.3f")) %>% 
  mutate(Intercept = NA,
         RSA = NA,
         model = "Simba2_B") %>% 
  select(model, Intercept, RSA, Hdiff, Vdiff, RSA:Hdiff, RSA:Vdiff)

bind_rows(coef_simba1, coef_simba2A, coef_simba2B) %>% 
  write.table(file = "tables/table_2.txt", sep = ",", quote = FALSE, row.names = FALSE)

