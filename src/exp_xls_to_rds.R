'
  Script: Organize publications and experiments meta data
  Input: messy .xls
  Output: .RDS file for organized publication and experiment information
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# upload data
temp_path <- "~/surfdrive/Work/PhD/mELA_materials/Eline/all_files_mELA_2020/5. Files/outcomes_mELA_VB_v3.xlsx"
dat_xl <- read_my_sheets(temp_path, c("publications", "experiments"))



# Included publications ---------------------------------------------------
publ_ft <- dat_xl$publications %>% 
  
  # only included and double checked so far
  filter(outcome_selection_final %in% c("include", "maybe_henk")) %>%
  
  # clean up df
  rename(id = PMID) %>% 
  
  # create cite var
  mutate(
    first_author = str_replace_all(authors, "de Melo", "de_Melo") %>% 
      str_replace_all("van der ", "van_der_") %>%
      str_replace_all("van ", "van_"),
    first_author = word(first_author, 1),
    cite = paste0(first_author, "(", year,")"), 
    cite = ifelse(duplicated(cite), paste0(cite, "b"), cite)) %>%
  dplyr::select(cite, id, authors, year, journal, title, link) 

# Experiments -------------------------------------------------------------
exp_ft <- dat_xl$experiments %>% 
  
  # get publ information
  left_join(publ_ft, by = "id") %>%
  
  filter(
    # only included publ
    id %in% publ_ft$id, 
    
    # only population of interest (keep females for secondary analysis)
    !strain %in% c("balbc", "other", "cd1", "swiss_webster"), # "not_specified"? long_evans_hooded, long_evans, lister_hooded
    sex != "not_specified"
  ) %>% 
  
  mutate(
    # population
    strain = ifelse(strain == "C57BL/6JRj", "c57bl6", strain),
    
    # intervention
    model_postdays = ifelse(model_start != model_end, 
                            paste(model_start, model_end, sep = "-"), 
                            model_start),
    model_control = case_when(
      model_control %in% c("short_separation", "brief_separation") ~ "animal_facility_reared",
      model_control %in% "not_available" ~ "not_applicable",
      model_control == "not_specified" & model == "limited_nesting" ~ "not_applicable",
      T ~ model_control
    ), 
    separation_repetition = str_remove_all(separation_repetition, "repeated_"),
    separation_light_phase = ifelse(separation_light_phase %in% c("not_available", "no"),
                                    "not_specified", separation_light_phase), 
    cross_fostering = ifelse(is.na(cross_fostering), "not_specified", cross_fostering), 
    
    # life events
    housing_after_weaning = ifelse(is.na(housing_after_weaning), "not_specified", housing_after_weaning),
    
    naive = case_when(
     is.na(other_life_experience) ~ "yes", 
     other_life_experience %in% c("normoxia exposure before sacrifice|home cage testing|sacrificed by perfusion|sacrificed by decapitation") ~ "yes",
     T ~ "no"
    ),
    
    behavior =  case_when(
      naive == "yes" ~ "no_behavior",
      str_detect(other_life_experience, "non-stressful|non stressful") ~ "non_stressful",
      str_detect(other_life_experience, "morris water|fear conditioning|behaviour including footshock|MWM|FST|swim test|step down inhibitory|cond_fear|forced swim|resident-intruder") ~ "stressful",
      str_detect(other_life_experience, "behavioral test|behavior tests 4/8 weeks|vaginal smears, behavior tests|behavior|von Frey hair test|behavior with") ~ "non_stressful", ## DOUBLE CHECK TYPE BEHAVIOR
      str_detect(other_life_experience, "EPM|dams transported pregnant, object recognition|first OF then OC|rotarod|elevated plus maze|open field|home cage testing") ~ "non_stressful",
      str_detect(other_life_experience, "forced swim test as stressor prior to decapitation") ~ "no_behavior", 
      T~ "no_behavior" ## double check if correct
    #  T~ other_life_experience ## double check if correct
    ),
    
   other_life_experience = str_replace_all(other_life_experience, 
                                           "footshock during experiment (fear conditioning)", 
                                           "footshock during experiment fear conditioning"),
    
    major_life_events = case_when(
      str_detect(housing_after_weaning, "single") ~ "yes",
      is.na(other_life_experience) ~ "no", 
      str_detect(other_life_experience, "anesthetics|microdialysis|anaesthesia|microchip implantation|stereotactic surgery|probe implantation") ~ "yes",
      other_life_experience %in% c("EPM, 10 min restraint stress prior to decapitation","EPM, 10 min restraint stress 15min prior to decapitation","behavior including MWM and foot-shock") ~ "no",
      str_detect(other_life_experience, "chronic restraint stress|variable stress|various stresses|unpredictable chronic stress|fox odor|stress immobilization|triple stressor|restraint stress|chronic constant light|chronic unpredictable stress|stressfull physical stimulation (3h)|short stress paradigm (5h)") ~ "yes",
      str_detect(other_life_experience, "footshock during experiment, housed in pairs|footshock during experiment, cond_fear|footshock during experiment fear conditioning") ~ "no", # already in stressful behavior
      str_detect(other_life_experience, "before chronic restraint") ~ "no", 
      str_detect(other_life_experience, "7day footshock|shocks during experiment|footshock during experiment") ~ "yes",
      str_detect(other_life_experience, "social_defeat") ~ "yes",
      str_detect(other_life_experience, "colorectal distension") ~ "yes",
      str_detect(other_life_experience, "blood collection tail|blood sampl|blood collection|blood from tail cut|cort sampling") ~ "yes",
      T~ "no" ## double check if correct
    #  T~ other_life_experience ## double check if correct
    ),
   
   check_cat_life_events = case_when(
     str_detect(other_life_experience, "single housed 2d before experiment|single housed for 2 days") ~ "yes",
     str_detect(other_life_experience, "housed with a female rat") ~ "yes",
     str_detect(other_life_experience, "single housing at 5 months") ~ "yes",
     str_detect(other_life_experience, "fasting / refeeding") ~ "yes", 
     str_detect(other_life_experience, "open field stress") ~ "yes",
     other_life_experience %in% c("acute stress cold water") ~ "yes", 
     T ~ "no"
     
   ),
   
   # about other life experiences
   origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0),
   behavior_num = ifelse(behavior == "stressful", 1, 0),
   chronic_num = ifelse(major_life_events == "yes", 1, 0), # single housing is currently not included
   trauma_score = rowSums(across(ends_with("_num"))), 
   trauma_presence = ifelse(trauma_score < 1, "no", "yes"),
    
    ## age (continues later)
    age_testing_start = str_remove_all(age_testing_start, "not_specified"), 
    age_testing_end = str_remove_all(age_testing_end, "not_specified")
    
  ) %>% 
  
  # age testing - get mean of start and end, convert to weeks - put together
  separate(age_testing_start, into = c("start_1", "start_2"), sep = "_", remove = FALSE)  %>% 
  separate(age_testing_end, into = c("end_1", "end_2"), sep = "_", remove = FALSE)  %>% 
  
  mutate(
    
    # get mean for ranges
    across(starts_with("start"), as.numeric),
    start = ifelse(is.na(start_2), start_1, start_1 + start_2/2),
    
    across(starts_with("end"), as.numeric),
    end = ifelse(is.na(end_2), end_1, end_1 + end_2/2),
    age_testing_unit = ifelse(id == "20816703", "months", age_testing_unit),
    
    # convert to weeks
    start = case_when(
      age_testing_unit == "months" ~ start * 4,
      age_testing_unit == "days" ~ start / 7,
      age_testing_unit == "weeks/days" ~ start,
      T ~ start
    ),
    end = case_when(
      age_testing_unit == "months" ~ end * 4,
      age_testing_unit == "days" ~ end / 7,
      age_testing_unit == "weeks/days" ~ end/7,

      T ~ end
    ), 
    
    # put together
    age_weeks = case_when(
      is.na(start) & is.na(end) ~ "not_specified",
      is.na(start) ~ as.character(round(end)), 
      is.na(end) ~ as.character(round(start)),
      round(start) == (end) ~ as.character(round(start)),
      T ~ paste(round(start), round(end), sep = "-")
    ), 
    
    # illegal exp design
    illegal_design =  ifelse(
      str_detect(other_life_experience, 
                 "enriched environment after weaning|hypoxia before sacrifice|running wheel exercise"),
      "yes", "no")
  ) %>% 
  filter(illegal_design %in% c("no", NA)) %>%
  rename(
    separation_hours = seperation_time
  ) %>%
  
  # keep relevant vars
  dplyr::select(
    
    # publication
    cite, id, authors, year, journal, title, link,
    exp_id,
    
    # about the population
    species, strain, origin, sex, other_life_experience,
    naive, behavior, major_life_events, housing_after_weaning, trauma_presence, check_cat_life_events,
    
    # about the intervention
    model, model_control, model_postdays, separation_hours,
    separation_repetition, separation_light_phase, 
    cross_fostering, litter_size, age_weeks
  ) %>% 
  
  # exp by mistake double
  filter(cite != "Mela(2015)b")


# Save temp data ----------------------------------------------------------
saveRDS(exp_ft, paste0(temp, "info_experiments.RDS"))
