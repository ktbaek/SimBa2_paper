library(tidyverse)

# after running python script, import data
rooman_simba <- read_csv("data/interim/ssym_2.csv")

rooman_simba %>% 
  group_by(Mut_index) %>% 
  mutate(count = n()) %>% 
  filter(count == 2) %>% #remove pairs with only direct or reverse represented
  fill(exp_ddG, .direction = "updown") %>% 
  mutate(exp_ddG = ifelse(Direction == "reverse", exp_ddG * -1, exp_ddG)) %>% 
  select(Mut_index, Direction, PDB, Gene, Chain, Number, Wild, RSA, Mutated, Hdiff, Vdiff, exp_ddG) %>% 
  mutate(dataset = "Ssym") %>% 
  write_csv("data/processed/ssym.csv")



  
  
  
  