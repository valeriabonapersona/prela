'
  Script: Prepare data for frequencies
  Input: all data
  Output: structured df with frequencies of each var of interest
  
  Author: v.bonapersona-2@umcutrecht.nl

'
# of note, this script supposes that you already run the previous source
# files in prela_paper

# Publications --------------------------------------------------------
freq_papers <- mab %>% 
  rename(exp_id = exp, outcome_id = each) %>% 
  rowwise() %>%
  mutate(outcome_id = paste0("mab_", outcome_id), 
         n_tot = sum(nC, nE), na.rm = T) %>%
  select(id, exp_id, outcome_id, n_tot, sex) %>% 
  bind_rows(plasma_epi %>% 
              rowwise() %>%
              mutate(n_tot = sum(n_c, n_e, na.rm = T)) %>%
              select(id, exp_id, outcome_id, n_tot, sex)) %>% 
  bind_rows(dat_full %>% 
              rowwise() %>%
              mutate(n_tot = sum(n_c, n_e, na.rm = T)) %>%
              select(id, exp_id, outcome_id, n_tot, sex)) %>% 
  mutate(
    sex = case_when(
      sex == "M" ~ "male", 
      sex == "F" ~ "female",
      T ~ sex)
  ) %>%
  unique() %>% 
  ungroup() 




