library(tidyverse)
library(magrittr)

prettify <- function(dataset) {
  
    data <- read_csv(paste0("data/", dataset, ".csv")) 
    
    data %<>% 
    mutate(
      RSA = sprintf("%.3f",round(RSA,3)),
      Hdiff = sprintf("%.1f",round(Hdiff,1)),
      Vdiff = sprintf("%.3f",round(Vdiff,3)))
    
    if("exp_ddG" %in% colnames(data)) {
      data %<>% 
        mutate(exp_ddG = sprintf("%.2f",round(exp_ddG,2)))
    } 
    
    data %>% 
    write_csv(paste0("data/Pretty_datasets/", dataset, ".csv"))
  
}

datasets <- c("B1112a",
              "B1112b",
              "B1112c",
              "B1112d",
              "B1112e",
              "B655",
              "S344",
              "Ssym",
              "B655_synon",
              "S344_synon")

map(datasets, prettify)
