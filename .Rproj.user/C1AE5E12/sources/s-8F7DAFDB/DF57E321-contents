library(tidyverse)
library(magrittr)

b1112 <- read_csv('data/B1112.csv')

# B1112a ------------------------------------------------------------------

b1112 %>% 
  mutate(dataset = "B1112a") %>% 
  write_csv("data/B1112a.csv")


# B1112b ------------------------------------------------------------------

wt_to_wt <- b1112 %>% 
  select(PDB, Wild, Number, RSA) %>% 
  distinct() %>% 
  mutate(
    Mutated = Wild,
    exp_ddG = 0,
    Hdiff = 0,
    Vdiff = 0)

b1112 %>% 
  bind_rows(wt_to_wt) %>% 
  arrange(PDB, Number, Mutated) %>% 
  mutate(dataset = "B1112b") %>% 
  write_csv("data/B1112b.csv")


# B1112c ------------------------------------------------------------------

set.seed(1127)

remove_points <- function(df, nremove) {
  
  df %<>% 
    sample_n(nremove) %>% 
    select(PDB, Number, Mutated) %>% 
    mutate(remain = FALSE)
  
  return(df)
  
}

b1112_bins <- b1112 %>% 
  mutate(bin = cut_width(exp_ddG, width = 0.5, center = 0)) %>% 
  group_by(bin) %>% 
  mutate(n = n(),
         over50 = ifelse(n - 50 < 0, 0, n - 50))

random_bin_points <- b1112_bins %>% 
  filter(over50 > 0) %>% 
  group_by(bin) %>% 
  group_map(~ remove_points(.x, unique(.x$over50))) %>% 
  bind_rows()

b1112c <- b1112 %>% 
  anti_join(random_bin_points, by = c("PDB", "Number", "Mutated")) %>% 
  mutate(dataset = "B1112c") %T>% 
  write_csv("data/B1112c.csv")


# B1112d ------------------------------------------------------------------

wt_to_wt <- b1112c %>% 
  select(PDB, Wild, Number, RSA) %>% 
  distinct() %>% 
  mutate(
    Mutated = Wild,
    exp_ddG = 0,
    Hdiff = 0,
    Vdiff = 0)

b1112c %>% 
  bind_rows(wt_to_wt) %>% 
  arrange(PDB, Number, Mutated) %>% 
  mutate(dataset = "B1112d") %>% 
  write_csv("data/B1112d.csv")


# B1112e ------------------------------------------------------------------

set.seed(1127)

b1112_bins <- b1112 %>% 
  mutate(bin = cut_width(exp_ddG, width = 0.5, center = 0)) %>% 
  group_by(bin) %>% 
  mutate(n = n()) %>% 
  mutate(bin = str_remove_all(bin, "\\(|\\)|\\[|\\]")) %>% 
  separate(bin, into = c("lo", "hi"), sep = ",") %>% 
  mutate(across(c(lo, hi), as.double)) %>% 
  rowwise() %>% 
  mutate(bin_mid = mean(c(lo, hi))) %>% 
  mutate(sign = sign(bin_mid), 
         bin = abs(bin_mid)) %>% 
  select(-lo, -hi, -bin_mid, -n)

remove_bin <- b1112_bins %>% 
  group_by(bin, sign) %>% 
  summarize(n = n()) %>% 
  filter(sign != 0) %>% 
  pivot_wider(names_from = sign, values_from = n) %>% 
  mutate(across(`-1`:`1`, ~ ifelse(is.na(.x), 0, .x))) %>%
  mutate(diff = `-1` - `1`) %>% 
  mutate(sign = -1 * sign(diff),
         diff = abs(diff)) %>% 
  select(bin, sign, diff)

random_bin_points <- b1112_bins %>% 
  full_join(remove_bin, by = c("bin", "sign")) %>% 
  filter(!is.na(diff)) %>% 
  group_by(bin, sign) %>%  
  group_map(~ remove_points(.x, unique(.x$diff))) %>% 
  bind_rows()

b1112 %>% 
  anti_join(random_bin_points, by = c("PDB", "Number", "Mutated")) %>% 
  mutate(dataset = "B1112e") %>% 
  write_csv("data/B1112e.csv")
