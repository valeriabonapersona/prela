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


# Harmonization across datasets -------------------------------------------
# change other in strain with actual strain?
all_data <- all_data %>% 
  
  mutate(
    remove_analysis = ifelse(is.na(remove_analysis), "no", remove_analysis),
    at_death = ifelse(
      at_death %in% c("novel Shock in shock-probe burial task", "3h after stressful physical stimulation"), "stressed",
      remove_analysis
    ), 
    domain = ifelse(outcome_id == "22922490_5_GLUT_1", "functional_gaba_glut", domain),
    ba_grouped = case_when(
      str_detect(brain_area_publication, "striatum|nucleus_accumb|pallid|caud|septum|bed_nucleus") ~ "striatum_and_pallidum",
      str_detect(brain_area_publication, "nucleus_accumb") ~ "nucleus_accumbens",
      str_detect(brain_area_publication, "caud") ~ "caudate_putamen",
      
      
      str_detect(brain_area_publication, "hippocamp|dentate|GZ|subiculum") ~ "hippocampal_region",
      brain_area_publication == "subventricular_zone" ~ "hippocampal_region",
      str_detect(brain_area_publication, "amygda") ~ "amygdala", 
      str_detect(brain_area_publication, "frontal") ~ "prefrontal_cortex",
      str_detect(brain_area_publication, "hypothal|mammilary|suprachiasmatic|incerta|optic|mammillary") ~ "hypothalamic_region",
      
      
      str_detect(brain_area_publication, "thalam|habenula|paraventricular_nucleus") ~ "thalamic_region",
      str_detect(brain_area_publication, "tegmental|substan|") ~ "vta_substancianigra",
      str_detect(brain_area_publication, "gray|raphe|midbrain|medulla|pons|brainstem|colliculus|tractus_solitarius|pontine") ~ "brainstem_midbrain_hindbrain",
      
      str_detect(brain_area_publication, "cerebellum") ~ "cerebellum",
      #   str_detect(brain_area_publication, "olfact") ~ "olfactory_areas",
      
      str_detect(brain_area_publication, "ventricles|edinger|internal_capsule|cortex|endopiri|olfact|cortical_layer") ~ "other_areas",
      #  str_detect(brain_area_publication, "cortex") ~ "cortex_other",
      #   str_detect(brain_area_publication, "medulla|pons|brainstem") ~ "brainstem",
      #  str_detect(brain_area_publication, "colliculus") ~ "colliculus",
      
      T ~ brain_area_publication
    ), 
    
    ba_main = case_when(
      str_detect(brain_area_publication, "striatum") ~ "striatum",
      str_detect(brain_area_publication, "nucleus_accumb") ~ "nucleus_accumbens",
      str_detect(brain_area_publication, "pallid") ~ "pallidum",
      str_detect(brain_area_publication, "caud") ~ "caudate_putamen",
      str_detect(brain_area_publication, "substant") ~ "sub_nigra",
      str_detect(brain_area_publication, "tegment") ~ "vta",
      
      
      str_detect(brain_area_publication, "hippocamp|dentate|GZ") ~ "hippocampus",
      str_detect(brain_area_publication, "amygda") ~ "amygdala", 
      str_detect(brain_area_publication, "frontal") ~ "prefrontal_cortex",
      str_detect(brain_area_publication, "hypothal|mammilary|suprachiasmatic|incerta|optic") ~ "hypothalamic_nuclei",
      
      
      str_detect(brain_area_publication, "thalam|habenula") ~ "thalamic_nuclei",
      str_detect(brain_area_publication, "raphe|midbrain|colliculus") ~ "midbrain",
      str_detect(brain_area_publication, "gray|medulla|ponsbrainstem|") ~ "brainstem_and_hindbrain",
      
      str_detect(brain_area_publication, "olfact") ~ "olfactory_areas",
      
      str_detect(brain_area_publication, "ventricles|edinger|internal_capsule|endopiri") ~ "other_areas",
      str_detect(brain_area_publication, "cortex") ~ "other_cortical",
      #   str_detect(brain_area_publication, "medulla|pons|brainstem") ~ "brainstem",
      #  str_detect(brain_area_publication, "colliculus") ~ "colliculus",
      
      T ~ brain_area_publication
    ),
    ba_location = case_when(
      str_detect(brain_area_publication, "dorsal") ~ "dorsal", 
      str_detect(brain_area_publication, "ventral") ~ "ventral", 
      str_detect(brain_area_publication, "basal") ~ "basal", 
      str_detect(brain_area_publication, "caudal") ~ "caudal", 
      str_detect(brain_area_publication, "apical") ~ "apical", 
      str_detect(brain_area_publication, "rostral") ~ "rostral", 
      str_detect(brain_area_publication, "medial") ~ "medial", 
      str_detect(brain_area_publication, "superficial") ~ "superficial", 
      str_detect(brain_area_publication, "deep") ~ "deep", 
      str_detect(brain_area_publication, "core") ~ "core", 
      
        T ~ "not_specified"
   #   T ~ brain_area_publication
    ), 
   
   across(c("estimate_c_type", "estimate_e_type"), 
          function(x) {
            case_when(
              x %in% c("average", "arbitrary_units") ~ "mean",
              is.na(x) ~ "not_available",
              T ~ x
              )}),
   
   across(c("deviation_c_type", "deviation_e_type"), 
          function(x) {
            case_when(
              x == "SE" ~ "sem",
              is.na(x) ~ "not_available",
              T ~ str_to_lower(x)
            )
            })
     
  ) %>% 
  select(-c(ba_layer, cell_type))



# Save data ---------------------------------------------------------------
saveRDS(all_data, paste0(temp, "prela_data_unprocessed.RDS"))
write.csv(all_data, paste0(temp, "prela_data_unprocessed.csv"))

