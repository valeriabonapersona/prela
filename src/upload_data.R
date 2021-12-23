'
  Script: Upload data
  Input: processed files of all meta-analyses
  
  Author: v.bonapersona-2@umcutrecht.nl

'

# Publications ------------------------------------------------------------
temp_path <- "~/surfdrive/Work/PhD/mELA_materials/Eline/all_files_mELA_2020/5. Files/outcomes_mELA_VB_v3.xlsx"
publ <- readxl::read_excel(temp_path, sheet = "publications")



# Data with effect sizes --------------------------------------------------
dat_full <- readRDS(paste0(final, "prela_effsize_complete.RDS"))
dat <- readRDS(paste0(final, "prela_effsize_aggr.RDS"))

# move this to preparation
dat <- dat %>% 
  mutate(at_death_grouped = ifelse(at_death == "stressed", "not_rest", 
                                   "rest"))
dat_full <- dat_full %>% 
  mutate(strain = ifelse(strain %in% c("c3hhenrj", "swiss_webster"), "other", strain))

# separate data for cvr
dat_cvr <- dat %>%
  select(-c(yi, vi)) %>% 
  rename(yi = yi_cvr, vi = vi_cvr) %>% 
  mutate(
    origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0), 
    major_life_events_num = ifelse(major_life_events == "yes", 1, 0), 
    behavior_num = ifelse(behavior == "stressful", 1, 0)
  ) %>% 
  rowwise() %>% 
  mutate(
    trauma_score = factor(sum(origin_num, major_life_events_num, behavior_num), levels = c("0", "1", "2", "3")),
    trauma_score = paste0("+", trauma_score)
  ) %>% 
  select(-c(ends_with("_num"))) %>%
  mutate(
    life_exp = case_when(
      trauma_score == "+0" & behavior %in% c("naive", "no_behavior") ~ "naive", 
      trauma_score == "+0" & behavior %in% c("non_stressful") ~ "non_stressful", 
      T ~ paste(trauma_score, "hit")
    )
  ) %>% 
  filter(!is.na(yi))

# Experiments' information ------------------------------------------------
exp <- readRDS(paste0(temp, "info_experiments.RDS"))


# Select only limbic ------------------------------------------------------
limbic <- dat %>% 
  filter(ba_grouped %in% c("amygdala", "prefrontal_cortex", "hippocampal_region"))

