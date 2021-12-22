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


# Experiments' information ------------------------------------------------
exp <- readRDS(paste0(temp, "info_experiments.RDS"))
