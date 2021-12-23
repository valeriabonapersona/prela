'
  Script: Prepare cfos meta-analysis results for use
  Input: output of cfos analysis (as from osf)
  Output: df with meta-analysis summaries from mab
  
  Author: v.bonapersona-2@umcutrecht.nl
'


# Environment preration ---------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")
source("src/upload_data.R")

cfos_meta <- readRDS(paste0(raw, "res_cfos_meta.RDS"))

cfos_meta_edit <- cfos_meta %>% 
  mutate(domain = "cfos", 
         trauma_presence = ifelse(hit =="Single", "no", "yes"), 
         at_death_grouped = ifelse(type == "Baseline", "rest", "not_rest")) %>% 
  rename(n_outcome_id = n, estimate = g) %>%
  rownames_to_column("remove") %>%
  select(-c(hit, type, remove)) %>% 
  relocate(domain, trauma_presence, at_death_grouped) %>% 
  mutate(
    sig = case_when(
      pval < 0.001 ~ "***", 
      pval < 0.01 ~ "**", 
      pval < 0.05 ~ "*", 
      T ~ ""
    )
  )

saveRDS(cfos_meta_edit, paste0(final, "cfos_meta_res.RDS"))
