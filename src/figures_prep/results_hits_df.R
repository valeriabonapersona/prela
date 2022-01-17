'
  Script: Merge results for figure limbic system hits
  Input: data from various analyses
  Output: results in df
  
  Author: v.bonapersona-2@umcutrecht.nl

'
## call it from somewhere else 

# Get results -------------------------------------------------------------
# epinephrine
source("src/analyses/epi_analysis.R")
res_epi_trauma_hit

#behavior
res_mab <- readRDS(paste0(final, "mab_meta_hit_res.RDS"))

#cfos whole brain
res_cfos <- readRDS(paste0(final, "cfos_meta_res.RDS"))
res_cfos <- res_cfos %>% 
  mutate(
    at_death_grouped = ifelse(at_death_grouped == "rest", "\n at rest", "\ after stress"),
    domain = paste(domain, at_death_grouped)) %>% 
  select(-at_death_grouped)
# morphology hits
res_morph <- readRDS(paste0(final, "morph_meta_res.RDS"))

res_hits <- res_epi_trauma_hit %>% 
  bind_rows(res_cfos) %>% 
  bind_rows(res_mab) %>% 
  bind_rows(res_morph)

rm(res_epi, res_mab, res_epi_trauma_hit, res_cfos, res_morph)