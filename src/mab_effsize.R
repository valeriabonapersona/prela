'
  Script: explore behavior availability
  Input: .RDS file (raw) with all behavior data
  Output: .RDS file (temp) with aggregated data and eff sizes
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

mab <- readRDS(paste0(raw, "mab.RDS"))

# Calculate effect estimates -----------------------------------------------
mab <- escalc("SMDH",
              m1i = mean_e, sd1i = sd_e, n1i = n_e,
              m2i = mean_c, sd2i = sd_c, n2i = n_c,
              data = mab)

# aggregate outcomes (due to time binning)
mab_aggr <- aggregate(mab, cluster = outcome_id, rho = 0.75) # rho high because expected to be very correlated


# Save data ---------------------------------------------------------------

saveRDS(mab_aggr, paste0(temp, "mab_aggr.RDS"))
