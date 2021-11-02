'
  Script: Cleaning manually generated data on immediate early genes
  
  Author: Valeria Bonapersona
  
  Input: excel file with relational structure, manually coded
  Output: immediate early genes data for prela
  
'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# upload data
temp_path <- "~/surfdrive/Work/PhD/mELA_materials/Eline/all_files_mELA_2020/5. Files/outcomes_mELA_VB_v3.xlsx"
dat_xl <- read_my_sheets(temp_path, c("publications", "experiments", "out_ieg"))



# Merge structure and bdnf ------------------------------------------------

ieg <- dat_xl$out_ieg[c(1:358),] %>% 
  
  # harmonize variables
  mutate(
    outcome = ifelse(outcome == "cFos", "c_fos", outcome), 
    product_measured = ifelse(product_measured == "mrna", "rna", "protein"),
    technique = case_when(
      str_detect(technique, "in_situ") ~ "in_situ",
      str_detect(technique, "pcr") ~ "pcr",
      T ~ technique
    ),
    days_after_induction = "not_applicable",
    # brain areas
    
    ## YOLO: continue with the grouping of brain areas. Then you are done with this
    brain_area_publication = str_replace_all(brain_area_publication, "nucleus_acceumbens", "nucleus_accumbens") %>%
      str_replace_all("amydala", "amygdala"),
    
    ba_grouped = case_when(
      str_detect(brain_area_publication, "striatum|nucleus_acc|pallid|caud|septum|stria_terminalis") ~ "striatum_and_pallidum",
      str_detect(brain_area_publication, "nucleus_accumb") ~ "nucleus_accumbens",
      str_detect(brain_area_publication, "caud") ~ "caudate_putamen",
      
      
      str_detect(brain_area_publication, "hippocamp|dentate|GZ|subiculum") ~ "hippocampus",
      str_detect(brain_area_publication, "amygda") ~ "amygdala", 
      str_detect(brain_area_publication, "frontal") ~ "prefrontal_cortex",
      str_detect(brain_area_publication, "hypothal|mammilary|mammillary|suprachiasmatic|incerta|optic|paraventricular_nucleus") ~ "hypothalamic_nuclei",
      
      
      str_detect(brain_area_publication, "thalam|habenula") ~ "thalamic_nuclei",
      str_detect(brain_area_publication, "gray|raphe|midbrain|medulla|pons|brainstem|colliculus|substant|tegmental|pontine") ~ "brainstem_midbrain_hindbrain",
      
      #   str_detect(brain_area_publication, "olfact") ~ "olfactory_areas",
      
      str_detect(brain_area_publication, "ventricles|edinger|internal_capsule|cortex|endopiri|olfact") ~ "other_areas",
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
      
      T ~ "not_specified"
    ), 
    ba_layer = "not_specified",
    
    part_cell = "not_applicable",
    
    distance_cell = "not_applicable", 
    
    # summ stats vars
    cut_n_c = ifelse(is.na(cut_n_c), "no", cut_n_c), 
    deviation_c_type = ifelse(deviation_c_type %in% c("SEM", "SE"), "sem", deviation_c_type),
    deviation_e_type = ifelse(deviation_e_type %in% c("SEM", "SE"), "sem", deviation_e_type),
    data_from = ifelse(is.na(data_from), "not_available", data_from),

    
    at_death = case_when(
      str_detect(acute_experience_description, "rest") ~ "rest",
      str_detect(acute_experience_description, "footshock|restraint|odor|FST|immobilization|forced swim test|MWM|social defeat|colorectal|colonorectal|inhibitory avoidance|competition") ~ "stressed",
      str_detect(acute_experience_description, "EPM|injection|fasting|20 min after behavior|new cage|non-stressful|experimental factors|open field|IGT|dark light box|three chamber|hypoxia chamber|social interaction|novel environment") ~ "aroused",
      acute_experience_description == "individual housing 1 day before testing" ~ "individual housing 1 day before testing",
       #  str_detect(other_life_experience)
      T ~ acute_experience_description
    ), 
    
    remove_analysis = ifelse(str_detect(note, "published earlier|exclude"), note, NA), 
    domain = "ieg", 
    out_grouped = ifelse(outcome == "c_fos", "c_fos", "other_ieg"), 
    data_unit_check = case_when( # check whether deviations are too large of these! Either exclude or sensitivity
      str_detect(data_unit, "fold|% of control|relative expression") ~ "yes", 
      T ~ "no"
    )
    
    ) %>% 
  
  rowwise() %>%
  mutate(
    age_testing = ifelse(age_testing == "not_specified", NA,
                         get_mean_range(age_testing)),
    age_testing_weeks = case_when(
      age_testing_unit == "months" ~ age_testing *4,
      age_testing_unit == "days" ~ age_testing/7,
      T ~ age_testing
    )
  ) %>% 
  ungroup() %>%
    
  # select vars of interest
  dplyr::select(
    remove_analysis,
    exp_id, outcome_id, double_outcomes,
    age_testing_weeks, at_death, domain,
    outcome, out_grouped, product_measured, technique, days_after_induction,
    brain_area_publication, ba_grouped, brain_area_hemisphere, ba_main, ba_location, ba_layer,
    part_cell, distance_cell,
    data_unit, data_unit_check, ends_with("_c"), ends_with("_e"),
    ends_with("_type"),
    sys_review_sig
  ) %>%
  unique()


# Save temp data ----------------------------------------------------------
saveRDS(ieg, paste0(temp, "ieg_prela.RDS"))
write.csv(ieg, paste0(temp, "ieg_prela.csv"))

