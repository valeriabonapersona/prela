'
  Script: Prepare morphology meta-analysis results for use
  Input: output of morphology analysis (as from osf)
  Output: df with meta-analysis summaries from mab
  
  Author: v.bonapersona-2@umcutrecht.nl
'


# Environment preration ---------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")
source("src/upload_data.R")

morph_meta_mod <- readRDS(paste0(temp, "morph_res_prela.RDS"))

morph <- dat %>% 
  filter(domain == "morphology", 
         out_grouped %in% c("length_dendrites", "branching", "volume"))


morph_meta_edit <- data.frame(
  domain = "morphology",
  trauma_presence = names(morph_meta_mod$test$coefficients),
  estimate = morph_meta_mod$test$coefficients,
  sd = morph_meta_mod$test$sigma, 
  tstat = morph_meta_mod$test$tstat, 
  pval = morph_meta_mod$test$pvalues
) %>% 
  filter(str_detect(trauma_presence, "trauma_presence")) %>% 
  mutate(trauma_presence = str_remove_all(trauma_presence, "trauma_presence")) %>%
  left_join(get_n(morph, "trauma_presence")) %>%
  mutate(
    ci_diff = abs((sd *3.92)/sqrt(n_outcome_id)),
    ci_low = estimate - ci_diff,
    ci_high = estimate + ci_diff, 
    se = 1.96*sd/n_outcome_id, 
    sig = case_when(
      pval < 0.001 ~ "***", 
      pval < 0.01 ~ "**", 
      pval < 0.05 ~ "*", 
      T ~ ""
    )
  ) %>% 
  select(-c(ci_diff, sd))

saveRDS(morph_meta_edit, paste0(final, "morph_meta_res.RDS"))
