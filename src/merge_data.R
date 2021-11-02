'
  Script: Merge together all data for full dataframe
  Input: preprocessed files from each meta-analysis project
  Output: .RDS file (temp) with all data
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# of note: at this stage the summary statistics have not been processed yet


# upload data
## experiments
exp <- readRDS(paste0(temp, "info_experiments.RDS"))

# source("~/surfdrive/Work/PhD/mMono/src/data_cleaning_prela.R") # to resave structural_plasticity_complete.RDS
mono <- readRDS("~/surfdrive/Work/PhD/mMono/data/temp/monoamines_complete_prela.RDS")
mono <- mono %>% mutate(cell_type = "not_applicable")

## structural plasticity
# source(here::here("~/surfdrive/Work/PhD/mESP/src/data_cleaning_prela.R")) # to resave structural_plasticity_complete.RDS
mesp <- readRDS("~/surfdrive/Work/PhD/mESP/data/temp/structural_plasticity_prela.RDS")

## gaba/glutamate
# source("~/surfdrive/Work/PhD/maGA/src/data_cleaning_prela.R") # to resave structural_plasticity_complete.RDS
gaglu <- readRDS("~/surfdrive/Work/PhD/maGA/data/temp/gaglu_prela.RDS")
gaglu <- gaglu %>% select(-outcome_data_type)

## IEG
#source("src/ieg_data_cleaning_prela.R")
ieg <- readRDS(paste0(temp, "ieg_prela.RDS"))
ieg <- ieg %>% mutate(cell_type = "not_applicable")


# Merge datasets ----------------------------------------------------------
all_data <- rbind(mono, mesp) %>% 
  rbind(gaglu) %>% 
  rbind(ieg) %>%
  data.frame() %>%
  filter(exp_id %in% exp$exp_id)

all_data <- exp %>% 
  right_join(all_data, by = "exp_id")
  

# missing exp id have been manually checked
# missing_exp <- exp$exp_id[!exp$exp_id %in% all_data$exp_id]
# temp_path <- "~/surfdrive/Work/PhD/mELA_materials/Eline/all_files_mELA_2020/5. Files/outcomes_mELA_VB_v3.xlsx"
# dat_xl <- read_my_sheets(temp_path, c("experiments"))
# dat <- dat_xl$experiments
# dat %>% 
#   filter(exp_id %in% missing_exp) %>% 
#   filter(!main_outcomes %in% c("behavior", "endocrine_markers", "labelling", "chip")) %>% 
#   pull(exp_id)



# Save data ---------------------------------------------------------------
saveRDS(all_data, paste0(temp, "prela_data_unprocessed.RDS"))
write.csv(all_data, paste0(temp, "prela_data_unprocessed.csv"))

