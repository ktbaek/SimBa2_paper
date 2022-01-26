library(tidyverse)
library(magrittr)

deep <- read_csv("data/deepddg_predict.csv") %>% 
  rename(DeepDDG_predict = Predicted)
dyna <- read_csv("data/dynamut_predict.csv") %>% 
  rename(Dynamut_predict = Predicted)


ssym_predict <- deep %>% 
  full_join(dyna, by = c("PDB",
                         "Chain",
                         "Number",
                         "Wild",
                         "Mutated")) %>% 
  full_join(read_csv("data/simba_sym_ssym_predict.csv"), by = c("Direction",
                                                                "PDB",
                                                                "Chain",
                                                                "Number",
                                                                "Wild",
                                                                "Mutated")) %>% 
  full_join(read_csv("data/simba_IB_ssym_predict.csv"), by = c("Direction",
                                                                "PDB",
                                                                "Chain",
                                                                "Number",
                                                                "Wild",
                                                                "Mutated")) %>% 
  select(Direction, PDB, Chain, Number, Wild, Mutated, exp_ddG, SimBa_IB_predict, SimBa_SYM_predict, DeepDDG_predict, Dynamut_predict) %>% 
  mutate(
    SimBa_IB_predict = as.double(sprintf("%.2f",round(SimBa_IB_predict,2))),
    SimBa_SYM_predict = as.double(sprintf("%.2f",round(SimBa_SYM_predict,2))),
    DeepDDG_predict = as.double(sprintf("%.2f",round(DeepDDG_predict,2))),
    Dynamut_predict = as.double(sprintf("%.2f",round(Dynamut_predict,2)))) %>% 
  write_csv("data/Pretty_datasets/Ssym_predict.csv")
  
