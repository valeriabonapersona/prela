'
  Script: Merge data mab and neurochemistry
  Input: processed files of meta-analyses
  Output: data organized for frequency graphs
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Preparation -------------------------------------------------------------
freq_vars <- c("species", "strain", "sex", "model", "age_testing_weeks")


# Select ------------------------------------------------------------------
freq_data <- dat_full %>% 
  select(ends_with("id"), all_of(freq_vars)) %>% 
  bind_rows(mab %>% select(ends_with("id"), all_of(freq_vars)))



