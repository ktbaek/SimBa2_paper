library(tidyverse)
library(magrittr)

# Test data sets
b655 <- read_csv('data/B655.csv') 
s344 <- read_csv('data/S344.csv') 

b655 %>% 
  select(PDB, Wild, Number, RSA) %>% 
  distinct() %>% 
  mutate(
    Mutated = Wild,
    Hdiff = 0,
    Vdiff = 0,
    dataset = "B655_synon") %>% 
  write_csv("data/b655_synon.csv")

s344 %>% 
  select(PDB, Wild, Number, RSA) %>% 
  distinct() %>% 
  mutate(
    Mutated = Wild,
    Hdiff = 0,
    Vdiff = 0,
    dataset = "S344_synon") %>% 
  write_csv("data/s344_synon.csv")
