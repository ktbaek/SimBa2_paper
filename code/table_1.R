library(tidyverse)
library(magrittr)
library(tidymodels)

# Read data sets

b655 <- read_csv('data/B655.csv')
s344 <- read_csv('data/S344.csv') 
ssym <- read_csv("data/ssym.csv")

b1112a <- read_csv('data/B1112a.csv')
b1112b <- read_csv('data/B1112b.csv')
b1112c <- read_csv('data/B1112c.csv')
b1112d <- read_csv('data/B1112d.csv')
b1112e <- read_csv('data/B1112e.csv')
  
mutation_types <- read_csv('data/HVdiff_table.csv')  %>% 
  select(Wild, Mutated)

mut_bias <- function(dataset){
  
  N <- dataset %>% 
    filter(Wild != Mutated) %>% 
    nrow()

  dataset %>% 
  select(Wild, Mutated) %>% 
  group_by_all() %>% 
  summarize(count = n()) %>% 
  full_join(mutation_types, by = c("Wild", "Mutated")) %>% 
  filter(Wild != Mutated) %>% 
  arrange(Wild, Mutated) %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>% 
  mutate(
    exp_frac = N / 380,
    norm_count = count / exp_frac
    ) %>% 
  ungroup() %>% 
  summarize(sd = sd(norm_count, na.rm = TRUE)) %>% 
  pull(sd)
  
}

stats <- function(dataset){
  
  mut_bias <- mut_bias(dataset)
  
  dataset %>% 
    summarize(
      N = n(),
      ddG_min = min(exp_ddG, na.rm = TRUE),
      ddG_max = max(exp_ddG, na.rm = TRUE),
      ddG_avg = mean(exp_ddG, na.rm = TRUE),
      ddG_sd = sd(exp_ddG, na.rm = TRUE),
      mut_bias = mut_bias
    )
  
}

datasets <- bind_rows(b1112a, b1112b, b1112c, b1112d, b1112e, b655, s344, ssym)

datasets %>% 
  group_by(dataset) %>% 
  nest() %>% 
  summarize(map_dfr(data, stats)) %>% 
  write.table(file = "tables/table_1.txt", sep = ",", quote = FALSE, row.names = FALSE)
