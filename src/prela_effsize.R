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
  
  # harmonize all summary stats
  mutate(
    
    # remove not specified everywhere
    across(.cols = c(ends_with("_c"), ends_with("_e")), 
           function(x)
             ifelse(x %in% c("NA", "not_specified"), NA, x)
           ),
    
    # sample size - keep low boundary, make numeric
    across(.cols = c("n_c", "n_e"), 
           function(x) 
             str_replace_all(x, "_.*", "") %>% as.numeric()
           ),
    
    # estimates - make numeric
    across(.cols = c("estimate_c", "estimate_e"), 
           function(x) as.numeric(x)
    )
    
  )

## YOLO: continue cleaning the summary statistics. Have a look at the estimates types and the deviation types. These should be corrected in the previous files.
  
  # convert all to same type of estimate and deviation
  rowwise() %>% 
  mutate(
    
    ## convert median to mean
    mean_c = case_when(
      estimate_c_type == "median" ~ median_to_mean(estimate_c,deviation_c),
      T ~ estimate_c),
    mean_e = case_when(
      estimate_e_type == "median" ~ median_to_mean(estimate_e,deviation_e),
      T ~ estimate_e),
    
    ## convert sem and iqr to sd
    sd_c = case_when(
      deviation_c_type == "iqr" ~ iqr_to_sd(estimate_c, deviation_c), 
      deviation_c_type %in% c("sem", "SEM", "SE") ~ sem_to_sd(deviation_c, n_c), 
      T ~ make_numeric(deviation_c)
    ),
    sd_e = case_when(
      deviation_e_type == "iqr" ~ iqr_to_sd(estimate_e, deviation_e), 
      deviation_e_type %in% c("sem", "SEM", "SE") ~ sem_to_sd(deviation_e, n_e), 
      T ~ make_numeric(deviation_e)
    )
    ) %>% 
    ungroup()

  
    ## here cut_n_c is only "no", so no correction needed + add missing sample sizes and deviations
    
  # missing data 
  

## calculate effect sizes (hedge's g and CVR)

# Calculate effect estimates -----------------------------------------------
mab <- escalc("SMDH",
              m1i = mean_e, sd1i = sd_e, n1i = n_e,
              m2i = mean_c, sd2i = sd_c, n2i = n_c,
              data = mab)

# aggregate outcomes (due to time binning)
mab_aggr <- aggregate(mab, cluster = outcome_id, rho = 0.75) # rho high because expected to be very correlated


# Save data ---------------------------------------------------------------

saveRDS(mab_aggr, paste0(temp, "mab_aggr.RDS"))
