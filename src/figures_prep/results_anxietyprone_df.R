'
  Script: Merge results for figures limbic system
  Input: data from various analyses
  Output: results in df
  
  Author: v.bonapersona-2@umcutrecht.nl

'

# Environment preparation -------------------------------------------------
source("src/general_funs.R")
source("src/upload_data.R")

# Get results -------------------------------------------------------------
# epinephrine
source("src/analyses/epi_analysis.R")

#behavior
source("src/analyses/mab_analysis.R")

#cfos amygdala
source("src/analyses/cfos_amyg_analysis.R")

# limbic system acute
source("src/analyses/limbic_situation_analysis.R")
res_bdnf <- res_limbic_acute %>% 
  filter(out_grouped == "bdnf", ba_grouped == "hippocampal_region") %>% 
  rename(n_outcome_id = n_out_id) %>% 
  select(-c(ba_grouped, domain)) 
anxietyprone_df <- res_epi %>% 
  bind_rows(res_mab) %>% 
  bind_rows(res_cfos_amyg) %>%
  filter(domain != "social") %>%
  mutate(
    out_grouped = ifelse(domain == "E", "plasma_epi", domain),
    at_death_grouped = case_when(
      domain %in% c("E", "cfos_rest") ~ "rest", 
      domain == "nonstressful_learning" ~ "rest", 
      domain %in% c("cfos_not_rest", "anxiety", "stressful_learning") ~ "not_rest", 
      
    )
    )%>% 
  select(-domain) %>%
  bind_rows(res_bdnf)


rm(res_epi, res_mab, res_limbic_acute, res_bdnf)