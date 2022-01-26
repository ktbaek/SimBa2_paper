l <- read_lines("data/interim/ssym_mut.txt")

df <- tibble(PDB = l) %>% 
  mutate(
    Chain = str_sub(PDB, 6, 6),
    PDB = str_sub(PDB, 1, 4)
  )
    

read_mut <- function(filepath) {
  
  pdb <- str_to_upper(str_sub(filepath, length(filepath)-11, length(filepath)-8))
  chain <- str_to_upper(str_sub(filepath, length(filepath)-6, length(filepath)-6))
  
  file_content <- read_lines(filepath)
  tibble(Mut = file_content) %>% 
    mutate(
      PDB = pdb,
      Chain = chain
    ) %>% 
    select(PDB, Chain, Mut) 
    
  
 
  
  #df <- tibble(data.frame(matrix(unlist(file_content), nrow = length(file_content), byrow=T)))
  
}

lapply(list.files("data/interim/Ssym_mut", full.names = TRUE), read_mut) %>% 
  bind_rows() %>% 
  write_csv("data/interim/dynamut.csv")

