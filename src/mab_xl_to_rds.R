'
  Script: Organize data on behavior
  Input: messy xl file with manual extraction
  Output: .csv and .RDS files harmonized (before preprocessing)
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# upload data
temp_path <- "~/surfdrive/Work/PhD/mELA_materials/Eline/all_files_mELA_2020/5. Files/outcomes_mELA_VB_v3.xlsx"
dat_xl <- read_my_sheets(temp_path, c("publications", "experiments", "out_behavior"))

# Select data of interest -------------------------------------------------
'
  Data of interest is in the out_behavior tab under "prela". So far we have included
  the hippocampal dependent tasks object in context and object in location, as well as fear conditioning (test phase).
  We still need to discuss whether to include the MWM, as its interpretation changes based on the water temperature
'
mab <- dat_xl$out_behavior %>% 
  
  # only hip dependent (and processed)
  filter(prela == "yes") %>% 
  
  # get exp and paper info
  left_join(dat_xl$experiments, by = "exp_id") %>% 
  left_join(dat_xl$publications %>% 
              rename(id = PMID) %>% 
              dplyr::select(-c("CHECK WITH EXP", starts_with("..."))), 
            by = "id") %>% 
  
  # create missing vars
  mutate(
    # for citation
    cite = paste0(word(authors, 1), "(", year,")"),
    cite = str_replace_all(cite, ",", ""),
    
    # for info about model
    model_postdays = ifelse(model_start != model_end, 
                            paste(model_start, model_end, sep = "-"), 
                            model_start),
    separation_light_phase = ifelse(separation_light_phase == "no", "not_applicable", 
                                    separation_light_phase), 
    housing_after_weaning = ifelse(str_detect(housing_after_weaning.x, "pair"), 
                                   housing_after_weaning.x, housing_after_weaning.y), 
    housing_after_weaning = ifelse(is.na(housing_after_weaning), 
                                   housing_after_weaning.y, housing_after_weaning), 
    age_testing_start = str_remove_all(age_testing_start, "not_specified"), 
    age_testing_end = str_remove_all(age_testing_end, "not_specified"), 
    sex = case_when(
      sex == "M" ~ "male", 
      sex == "F" ~ "female", 
      T ~ sex
    ), 
    
    # about the outcome
    retention_hours = ifelse(retention_to_test_unit == "min", 
                             as.numeric(retention_to_test) / 60, retention_to_test), 
    habituation = case_when(
      habituation %in% c("not_applicable") ~ "not_specified", 
      str_detect(habituation, "room") ~ "room", 
      str_detect(habituation, "arena") ~ "arena", 
      T ~ habituation
    ), 
    
    # about other life experiences
    origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0),
    behavior_num = ifelse(behavior == "stressful", 1, 0),
    chronic_num = ifelse(major_life_events == "yes", 1, 0), # single housing is together with major life events
    trauma_score = rowSums(across(ends_with("_num"))), 
    trauma_presence = ifelse(trauma_score < 1, "no", "yes"),

    # summary stats
    cut_n_c = ifelse(is.na(cut_n_c), "no", cut_n_c), 
    n_c = str_replace_all(n_c, "_.*", "") %>% make_numeric(), # keep lowest
    n_e = str_replace_all(n_e, "_.*", "") %>% make_numeric(), # keep lowest
    sd_c = case_when(
      #   deviation_c_type == "iqr" ~ iqr_to_sd(estimate_c, deviation_c), 
      deviation_c_type %in% c("sem", "SEM", "SE") ~ sem_to_sd(deviation_c, n_c), 
      T ~ make_numeric(deviation_c)
    ),
    sd_e = case_when(
      #   deviation_e_type == "iqr" ~ iqr_to_sd(estimate_e, deviation_e), 
      deviation_e_type %in% c("sem", "SEM", "SE") ~ sem_to_sd(deviation_e, n_e), 
      T ~ make_numeric(deviation_e)
    )
  ) %>% 
  
  # create var for age testing - get mean of start and end, convert to weeks - put together
  separate(age_testing_start, into = c("start_1", "start_2"), sep = "_")  %>% 
  separate(age_testing_end, into = c("end_1", "end_2"), sep = "_")  %>% 
  
  mutate(
    
    # get mean for ranges
    across(starts_with("start"), as.numeric),
    start = ifelse(is.na(start_2), start_1, start_1 + start_2/2),
    
    across(starts_with("end"), as.numeric),
    end = ifelse(is.na(end_2), end_1, end_1 + end_2/2),
    
    # convert to weeks
    start = case_when(
      age_testing_unit.y == "months" ~ start * 4,
      age_testing_unit.y == "days" ~ start / 7,
      T ~ start
    ),
    end = case_when(
      age_testing_unit.y == "months" ~ end * 4,
      age_testing_unit.y == "days" ~ end / 7,
      T ~ end
    ), 
    
    # put together
    age_weeks = case_when(
      is.na(start) ~ "", 
      is.na(end) ~ as.character(round(start)), 
      T ~ paste(round(start), round(end), sep = "-")
    )
  ) %>%

  rename(
    separation_hours = seperation_time,
    
    # all estimates have the same type
    mean_c = estimate_c,
    mean_e = estimate_e,
    sem_c = deviation_c,
    sem_e = deviation_e
    ) %>%
  
  # keep only vars of interest
  select(
    # about publications
    cite, link, id, title, exp_id, outcome_id,
    
    # about the population
    species, strain, origin, sex, other_life_experience,
    naive, behavior, major_life_events, trauma_presence,
    
    # about the intervention
    model, model_control, model_postdays, separation_hours,
    separation_repetition, separation_light_phase, 
    cross_fostering, litter_size, age_weeks, 
    
    # about the outcome
    outcome, retention_hours, light_dark_phase, handling, 
    habituation, comment_behavior, 
    
    # statistics
    cut_n_c, ends_with("_c"), ends_with("_e"), data_unit
  )




# Save --------------------------------------------------------------------
saveRDS(mab, paste0(raw,"mab.RDS"))
write.csv(mab, paste0(raw,"mab.csv"))
