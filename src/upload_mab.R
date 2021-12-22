'
  Script: Upload meta-analysis of behavior data
  Input: data as published in Bonapersona (2019), Neuro & Biobeh reviews
  Output: data as object
  
  Author: v.bonapersona-2@umcutrecht.nl

'


load("~/surfdrive/Work/PhD/MAB/MAB_Analysis/MAB_Analysis_submitGit/data.RData")
mab <- data %>%
  filter(domain != "noMeta") %>%
  droplevels()

# strain
mab <- mab %>% 
  mutate(strain = str_replace_all(strainGrouped, "E", "_e") %>%
           str_replace_all("H", "_h") %>% 
           str_replace_all("eD", "e_d") %>% 
           str_replace_all("W", "_w") %>%
           str_replace("K", "_k") %>%
           tolower(), 
         strain = ifelse(strain %in% c("c3hhenrj", "nmri", "dba", "wistar_kyoto", "ns"), "other", strain),
         model = case_when(
           model == "I" ~ "isolation", 
           model == "LG" ~ "licking_grooming", 
           model == "LNB" ~ "limited_nesting", 
           model == "M" ~ "maternal_separation", 
           model == "MD" ~ "maternal_deprivation", 
           T ~ "check"
         ),
         outcome_id = paste0("mab_", each)) %>% 
  rename(exp_id = exp, age_testing_weeks = ageWeekNum)
