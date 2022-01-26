library(tidyverse)
library(magrittr)
library(tidymodels)

rsquared <- function(x,y) {
    m <- lm(y ~ x)
    return(summary(m)$r.squared)
  }


get_rmse <- function(exp, pred) {

  yardstick::rmse(df, truth = exp, estimate = pred)$.estimate
  
}

delta_average <- function(df) {
  
  df %>% 
    select(Mut_index, Direction, Predicted) %>% 
    pivot_wider(names_from = Direction, values_from = Predicted) %>% 
    mutate(delta = direct + reverse) %>% 
    summarize(delta_avg = mean(delta, na.rm = TRUE)) %>% 
    pull(delta_avg)
  
}

corr_coef <- function(df) {
  
  df %>% 
    select(Mut_index, Direction, Predicted) %>% 
    pivot_wider(names_from = Direction, values_from = Predicted) %>% 
    mutate(R = -1 * sqrt(rsquared(direct, reverse))) %>% 
    pull(R) %>% 
    unique()
  
}

dyna_df <- read_csv("github_repo/data/dynamut.csv") %>% 
  mutate(
    Number = as.integer(str_sub(Mut, 2, nchar(Mut) - 1)),
    Wild = str_sub(Mut, 1, 1),
    Mutated = str_sub(Mut, nchar(Mut), nchar(Mut))
  ) %>% 
  select(-Mut) %T>%
  write_csv("github_repo/data/dynamut_predict.csv")
  
ssym <- read_csv("github_repo/data/ssym.csv")

df <- ssym %>% 
  select(-RSA, -Hdiff, -Vdiff, -dataset) %>% 
  left_join(dyna_df, by = c("PDB", "Chain", "Number", "Wild", "Mutated"))

#check, should be FALSE
any(is.na(df$Predicted))

#which if any are missing
df %>% 
  filter(is.na(Predicted))

df_2 <- df %>% 
  rename(Experimental = exp_ddG) %>% 
  group_by(Direction) %>% 
  mutate(
    R = sqrt(rsquared(Experimental, Predicted)),
    RMSE = get_rmse(Experimental, Predicted)
    )

delta_avg <- delta_average(df_2)
corr <- corr_coef(df_2)

df_2 %>% 
  select(Direction, R, RMSE) %>% 
  distinct() %>% 
  pivot_wider(names_from = Direction, values_from = c(R, RMSE)) %>% 
  mutate(
    delta_avg = delta_avg,
    Rdirinv = corr,
    model = "Dynamut") %>% 
  mutate(across(where(is.numeric), sprintf, fmt = "%.2f")) %>% 
  select(model, RMSE_direct, R_direct, RMSE_reverse, R_reverse, Rdirinv, delta_avg)