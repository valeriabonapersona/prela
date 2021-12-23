'
  Script: Prepare mab meta-analysis results for use
  Input: output of mab analysis (as from osf)
  Output: df with meta-analysis summaries from mab
  
  Author: v.bonapersona-2@umcutrecht.nl
'


# Environment preration ---------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")
source("src/upload_data.R")

mab_meta <- readRDS(paste0(raw, "prela_mab_res.RDS"))

source("src/upload_mab.R")

mab_ft <- mab %>% 
  filter(sex == "M") %>% 
  mutate(trauma_presence = ifelse(hit2Grouped == "1", "yes", "no"))

mab_meta_edit <- mab_meta %>%
  rename(meta_test = test, 
         zval = Zvalue, 
         pval = Pvalue_bonfCorr, 
         estimate = effectsizeCorrected, 
         ci_low = ci.lb_Corrected,
         ci_high = ci.ub_Corrected) %>% 
  filter(!str_detect(meta_test, "Hit")) %>%
  select(meta_test, estimate, se,zval, pval, ci_low, ci_high)

#saveRDS(mab_meta_edit, paste0(final, "mab_meta_res.RDS"))

mab_meta_hit<- mab_meta %>%
  filter(str_detect(test, "Yes|No")) %>% 
  mutate(trauma_presence = ifelse(str_detect(test, "Yes"), "yes", "no")) %>%
  rename(zval = Zvalue, 
         pval = Pvalue_bonfCorr, 
         estimate = effectsizeCorrected, 
         ci_low = ci.lb_Corrected,
         ci_high = ci.ub_Corrected) %>% 
  select(domain, trauma_presence, estimate, se,zval, pval, ci_low, ci_high) %>% 
  mutate(
    domain = case_when(
      domain == "sLearning" ~ "stressful_learning", 
      domain == "nsLearning" ~ "nonstressful_learning", 
      T ~ domain
    ),
    sig = case_when(
      pval < 0.001 ~ "***", 
      pval < 0.01 ~ "**", 
      pval < 0.05 ~ "*", 
      T ~ ""
    ) 
  ) %>% 
  left_join(get_n(mab_ft, "trauma_presence"))
saveRDS(mab_meta_hit, paste0(final, "mab_meta_hit_res.RDS"))
