library(tidyverse)

ssym <- read_csv("data/raw/datasets/ssym_Pucci.csv")

# check data integrity
ssym %>% 
  mutate(chain = Chain_d == Chain_r,
         aa1 = Wild_d == Mutated_r,
         aa2 = Wild_r == Mutated_d,
         no = Number_d == Number_r) %>% 
  rowwise() %>% 
  filter(!all(c(chain, aa1, aa2, no)))  # there are two where the chains don't match 

# tidy dataframe and export to csv
ssym %>% 
  rename(Mut_index = X1) %>% 
  select(-PubMedID, -pH, -Temp, -Number_r) %>% 
  pivot_longer(c(PDB_d, PDB_r), names_to = c(NA, "Direction"), names_sep = "_", values_to = "PDB") %>% 
  mutate(Chain = ifelse(Direction == "d", Chain_d, Chain_r)) %>% 
  rename(
    Number = Number_d) %>% 
  mutate(
    Direction = case_when(
      Direction == "d" ~ "direct",
      Direction == "r" ~ "reverse"
    )) %>% 
  mutate(exp_ddG = ifelse(Direction == "reverse", NA, exp_ddG)) %>% 
  mutate(Wild = ifelse(Direction == "direct", Wild_d, Mutated_d)) %>% 
  mutate(Mutated = ifelse(Direction == "direct", Wild_r, Mutated_r)) %>% 
  select(-Wild_d, -Mutated_d, -Wild_r, -Mutated_r) %>% 
  select(Mut_index, Direction, Chain, Number, PDB, Wild, Mutated, exp_ddG) %>% 
  write_csv("data/ssym_tidy.csv")
