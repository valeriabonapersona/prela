'
  Script: Organize data on monoamines
  Input: preprocessed file from mMono project
  Output: .RDS file (temp) with aggregated data and eff sizes
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# upload data
# source("~/surfdrive/Work/PhD/mESP/src/data_cleaning.R") # to resave structural_plasticity_complete.RDS
mono <- readRDS("~/surfdrive/Work/PhD/mMono/data/temp/monoamines_complete.RDS")

exp <- readRDS(paste0(temp, "info_experiments.RDS"))


# Prepare meta data -------------------------------------------------------
mono_ft <- mono %>% 
  
  # keep only relevant
  filter(exp_id %in% exp$exp_id,
         ba_grouped == "hippocampus") %>% 
  select(
    # general vars
    exp_id, outcome_id, 
    # life events
    at_death, 
    # outcome
    outcome, out_grouped, product_measured, technique, 
 #   days_after_induction, 
    # brain area
    brain_area_publication, brain_area_hemisphere, ba_main,
  #  distance_cell, 
    # stats
    data_unit, ends_with("_c"), ends_with("_e")
  ) %>% 
  
  # clean
  rename(
    out_type = out_grouped,
    out_grouped = outcome
  ) %>%
  
  # clean inconsistencies
  mutate(
    product_measured = ifelse(product_measured == "total_neurons", 
                              "count", product_measured), 
    
    ## keep all bdnf separately
    out_type = ifelse(out_type == "D1Rlike", 
                         "receptors_and_transporters",
                      out_type),
    
    # column to later cluster eff sizes > 1 value per outcome per experiment
    aggr = paste0(exp_id, out_grouped, product_measured)
  )

# put together with exp meta-data
mono_cleaned <- exp %>% 
  right_join(mono_ft, by = "exp_id") %>% 
  arrange(aggr)


# Calculate effect estimates -----------------------------------------------
# calculate effect sizes 
## hedges g
mono_cleaned <- escalc("SMDH",
                       m1i = mean_e, sd1i = sd_e, n1i = n_e,
                       m2i = mean_c, sd2i = sd_c, n2i = n_c,
                       data = mono_cleaned, var.names = c("yi_g", "vi_g"))


# aggregate outcomes (subparts of same area)
mono_aggr <- aggregate(mono_cleaned, cluster = aggr, rho = 0.75)


# Save data ---------------------------------------------------------------
saveRDS(mono_aggr, paste0(temp, "mono_aggr.RDS"))
