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
mono <- mono %>% 
  mutate(
    cell_type = "not_applicable", 
    aggr = ifelse(!double_outcomes == "NA", paste(double_outcomes, "aggr", sep = "_"),
                  double_outcomes)
    ) %>% 
  select(-double_outcomes)

## structural plasticity
# source(here::here("~/surfdrive/Work/PhD/mESP/src/data_cleaning_prela.R")) # to resave structural_plasticity_complete.RDS
mesp <- readRDS("~/surfdrive/Work/PhD/mESP/data/temp/structural_plasticity_prela.RDS")

## gaba/glutamate
# source("~/surfdrive/Work/PhD/maGA/src/data_cleaning_prela.R") # to resave structural_plasticity_complete.RDS
gaglu <- readRDS("~/surfdrive/Work/PhD/maGA/data/temp/gaglu_prela.RDS")
gaglu <- gaglu %>% 
  select(-outcome_data_type)

## IEG
#source("src/ieg_data_cleaning_prela.R")
ieg <- readRDS(paste0(temp, "ieg_prela.RDS"))
ieg <- ieg %>% 
  rename(aggr = double_outcomes) %>%
  mutate(
    cell_type = "not_applicable", 
    domain = out_grouped,
    out_grouped = case_when(
      str_detect(outcome, "erg|egr") ~ "egr_variants", 
      str_detect(outcome, "fos_b") ~ "fosb_variants", 
      T ~ outcome
    )
  )


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
      at_death
    ), 
    strain = case_when(
      strain == "other" & id == "26548415" ~ "balbc/c57bl6",
      strain == "other" & id == "30872090" ~"c3hhenrj",
      T ~ strain
    ),
    domain = ifelse(outcome_id == "22922490_5_GLUT_1", "functional_gaba_glut", domain),
    
    ba_grouped = case_when(
      str_detect(brain_area_publication, "striatum") ~ "striatum",
      str_detect(brain_area_publication, "nucleus_accumb") ~ "nucleus_accumbens",
      str_detect(brain_area_publication, "pallid|stria_terminalis|septum_lateral|septum") ~ "pallidum",
      str_detect(brain_area_publication, "caudat|caudo_putamen") ~ "caudate_putamen",
      str_detect(brain_area_publication, "substant|substancia_nigra") ~ "sub_nigra",
      str_detect(brain_area_publication, "tegment") ~ "vta",
      
      
      str_detect(brain_area_publication, "hippocamp|dentate|GZ|subiculum_ventral") ~ "hippocampal_region",
      str_detect(brain_area_publication, "amygda") ~ "amygdala", 
      str_detect(brain_area_publication, "frontal") ~ "prefrontal_cortex",
      str_detect(brain_area_publication, "hypothal|mammilary|suprachiasmatic|incerta|optic|mammillary|subventricular_zone") ~ "hypothalamic_region",
      
      
      str_detect(brain_area_publication, "thalam|habenula|paraventricular_nucleus") ~ "thalamic_region",
      str_detect(brain_area_publication, "raphe|midbrain|colliculus") ~ "midbrain",
      str_detect(brain_area_publication, "gray|medulla|pons|brainstem|pontine|locus_coeruleus") ~ "brainstem_hindbrain",
      str_detect(brain_area_publication, "tractus_solitarius") ~ "medulla",
      str_detect(brain_area_publication, "olfact") ~ "olfactory_areas",
      
      str_detect(brain_area_publication, "ventricles|edinger|internal_capsule|endopiri") ~ "other_areas",
      str_detect(brain_area_publication, "cortex|cortical_layer") ~ "other_cortical",
      #   str_detect(brain_area_publication, "medulla|pons|brainstem") ~ "brainstem",
      #  str_detect(brain_area_publication, "colliculus") ~ "colliculus",
      str_detect(brain_area_publication, "cerebellum") ~ "cerebellum",
      str_detect(brain_area_publication, "not_specified") ~ "NA",
      
      T ~ brain_area_publication
    ),
    ba_grouped = ifelse(str_detect(brain_area_publication, 
                                "orbital_frontal_agranular_insular_cortex_l"), 
                     "other_cortical", ba_grouped),
    ba_location = case_when(
      brain_area_publication == "ventral_tegmental_area" ~ "",
      str_detect(brain_area_publication, "dorsal") ~ "dorsal", 
      str_detect(brain_area_publication, "ventral") ~ "ventral", 
      str_detect(brain_area_publication, "basal") ~ "basal", 
      str_detect(brain_area_publication, "caudal") ~ "caudal", 
      str_detect(brain_area_publication, "apical") ~ "apical", 
      str_detect(brain_area_publication, "rostral") ~ "rostral", 
      str_detect(brain_area_publication, "medial") ~ "medial", 
      str_detect(brain_area_publication, "superficial") ~ "superficial", 
      str_detect(brain_area_publication, "intermediate") ~ "intermediate", 
      str_detect(brain_area_publication, "deep") ~ "deep", 
      str_detect(brain_area_publication, "core") ~ "core", 
      str_detect(brain_area_publication, "shell") ~ "shell", 
      str_detect(brain_area_publication, "_central") ~ "central", 
      str_detect(brain_area_publication, "_lateral") ~ "lateral", 
      str_detect(brain_area_publication, "basolateral") ~ "basolateral",
      
      
        T ~ ""
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
            }), 
   
   
   # correction typos values 
   deviation_e = ifelse(outcome_id %in% paste0("17561822_1_bdnf_", c(1:6, 11)), 
                 abs(as.numeric(as.character(deviation_e)) - 
                       as.numeric(as.character(estimate_e))), deviation_e), 
   deviation_e = ifelse(outcome_id %in% paste0("12140784_1_bdnf_", c(1:3, 7)), 
                        abs(as.numeric(as.character(deviation_e))), deviation_e)
     
  ) %>% 
  select(-c(ba_layer, cell_type)) %>% 
  
  # correction typos found at a later stage
  filter(age_testing_weeks < 53)


# Save data ---------------------------------------------------------------
saveRDS(all_data, paste0(temp, "prela_data_unprocessed.RDS"))
write.csv(all_data, paste0(temp, "prela_data_unprocessed.csv"))

