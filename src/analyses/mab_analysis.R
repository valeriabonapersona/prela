'
  Script: Re-analysis behavior
  Input: data as published in Bonapersona (2019), Neuro & Biobeh reviews
  Output: results in df
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Upload data -------------------------------------------------
load("~/surfdrive/Work/PhD/MAB/MAB_Analysis/MAB_Analysis_submitGit/data.RData")
mab <- data %>%
  filter(domain != "noMeta") %>%
  droplevels()

mab_ft <- mab %>% 
  filter(sex == "M")


# Behavior -------------------------------------------------------------
mod_mab <- rma.mv(yi, vi,
                  random = list(~1 | each, ~1 | exp),
                  mods   = ~domain - 1 ,
                  data = mab_male)
res_mab <- mod_to_df(mod_mab)
res_mab <- mab_male %>% 
  group_by(domain) %>% 
  summarize(
    n_id = length(unique(id)), 
    n_exp_id = length(unique(exp)), 
    n_outcome_id = length(id)
  ) %>% 
  cbind(res_mab) %>% 
  relocate(c("n_id", "n_exp_id", "n_outcome_id"), .after = "sig") %>% 
  mutate(
    domain = case_when(
      domain == "sLearning" ~ "stressful_learning",
      domain == "nsLearning" ~ "nonstressful_learning", 
      T ~ as.character(domain)
    ),
    estimate = ifelse(domain %in% c("nonstressful_learning", "social"), -1*estimate, estimate)
  ) %>% 
  `rownames<-`( NULL )

rm(mod_mab, mab_male)