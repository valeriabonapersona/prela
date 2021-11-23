'
  Script: clean prela data
  Input: .RDS file with all prela data (from merge_data.R)
  Output: .RDS file (temp) with aggregated data and eff sizes
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

prela_unprocessed <- readRDS(paste0(temp, "prela_data_unprocessed.RDS"))



# Summary statistics cleaning ---------------------------------------------
prela_cleaned <- prela_unprocessed %>% 
  
  # remove missing values and not meta-analyzable data
  filter(
    !remove_analysis %in% c("yes", "exclude?", "extremely skewed medians")
  ) %>%
  # harmonize all summary stats
  mutate(
    
    # remove not specified everywhere
    across(.cols = c(ends_with("_c"), ends_with("_e")), 
           function(x)
             ifelse(x %in% c("NA", "not_specified", "not_specfied", "not_available"), NA, x)
           ),
    
    # sample size - keep low boundary, make numeric
    across(.cols = c("n_c", "n_e"), 
           function(x) 
             str_replace_all(x, "_.*", "") %>% as.numeric()
           ),
    
    # estimates - make numeric
    across(.cols = c("estimate_c", "estimate_e"), 
           function(x) as.numeric(x)
    ), 
    
    # convert medians to means
    mean_c = case_when(
      estimate_c_type == "median" ~ median_to_mean(estimate_c,deviation_c),
      T ~ estimate_c),
    mean_e = case_when(
      estimate_e_type == "median" ~ median_to_mean(estimate_e,deviation_e),
      T ~ estimate_e), 
    
    # add missing sample sizes for later conversion sem to sd
    across(c("n_c", "n_e"), function(x)
      ifelse(is.na(x), median(x, na.rm = T), x)
      ),

    # convert sem and iqr to sd
    sd_c = case_when(
      deviation_c_type == "iqr" ~ iqr_to_sd(estimate_c, deviation_c), 
      deviation_c_type %in% c("sem", "not_specified") ~ sem_to_sd(deviation_c, n_c), 
      T ~ make_numeric(deviation_c)
    ),
    sd_e = case_when(
      deviation_e_type == "iqr" ~ iqr_to_sd(estimate_e, deviation_e), 
      deviation_e_type %in% c("sem", "not_specified") ~ sem_to_sd(deviation_e, n_e), 
      T ~ make_numeric(deviation_e)
    )
  ) %>% 
  ungroup() %>% 
  
  # cut n_c if control had multiple exp groups
  mutate(
    n_c = ifelse(cut_n_c == "yes", n_c/2, n_c),
    
    # correct typo
    mean_c = ifelse(outcome_id %in% c("22693577_1_IEG1", "22693577_1_IEG2"),
                    NA, mean_c)
  ) %>%
  select(-c(ends_with("_type"), starts_with("estimate_"), starts_with("deviation_"))) 


# calculate CVR eff size --------------------------------------------------
## needs to be calculated before deviations have been assumed identical

## why cannot it be computed without means?
prela_cvr <- escalc("CVR",
                  m1i = mean_e, sd1i = sd_e, n1i = n_e,
                  m2i = mean_c, sd2i = sd_c, n2i = n_c,
                  data = prela_cleaned, var.names = c("yi_cvr", "vi_cvr"))

prela_cvr <- prela_cvr %>% 
  select(outcome_id, yi_cvr, vi_cvr)



# calculate SMD effect sizes (hedge's g) -----------------------------------------------
## preprocessing

prela_cleaned <- prela_cleaned %>% 
  mutate(
    # incorrect data_unit
    data_unit = case_when(
      outcome_id %in% c("20463226_2_neuro_7", 
                        paste0("25159716_1_bdnf_", c(10, 11, 21, 22, 32, 33)), 
                        paste0("25159716_2_bdnf_", c(10, 11, 21, 22, 32, 33)), 
                        paste0("26382238_2_bdnf_", c(1:2)), 
                        paste0("17164818_2_bdnf_", c(1:20)), 
                        paste0("17164818_3_bdnf_", c(1:20))) ~ "not specified", 
      outcome_id %in% c("23237316_4_bdnf_1","24802968_1_bdnf_1", 
                        paste0("24802968_1_bdnf_", c(1:3, 12:14)),
                        paste0("24802968_2_bdnf_", c(1:3, 12:14))
      ) ~ "fold change",
      T ~ data_unit), 
    
    # mitigate fold changes and percentages >> IMPROPER DEALING WITH FOLD CHANGES
    fold_change = case_when(
      str_detect(data_unit,"fold change|fold_change|fold difference|ratio_control") ~ mean_e,
      data_unit == "perc_change_control" ~ (100 + mean_e) / 100,
      str_detect(data_unit, "perc") ~ mean_e / 100, 
      T ~ -100
    ),
    fold_change = ifelse(fold_change == -100, NA, fold_change), 
    fold_change_dev = case_when(
      str_detect(data_unit,"fold change|fold_change|fold difference|ratio_control") ~ sd_e,
      str_detect(data_unit, "perc") ~ sd_e / 100, 
      T ~ -100
    ),
    fold_change_dev = ifelse(fold_change_dev == -100, NA, fold_change_dev),
    
    # assume that deviations are the same if missing
    sd_c = ifelse(is.na(sd_c), sd_e, sd_c),
    sd_e = ifelse(is.na(sd_e), sd_c, sd_e), 
    mean_c = case_when(
      is.na(mean_c) & data_unit == "perc_change_control" ~ 0,
      is.na(mean_c) & data_unit == "fold change (ELA - HC)" ~ 1, 
      T ~ mean_c
    ),
    missing_data = ifelse(is.na(mean_e), "yes", "no")
  )

prela_g <- escalc("SMDH",
              m1i = mean_e, sd1i = sd_e, n1i = n_e,
              m2i = mean_c, sd2i = sd_c, n2i = n_c,
              data = prela_cleaned)



prela_effsize <- prela_g %>% 
  cbind(prela_cvr[,-1])


# Save data ---------------------------------------------------------------

saveRDS(prela_effsize, paste0(final, "prela_effsize.RDS"))
