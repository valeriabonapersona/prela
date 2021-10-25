'
  Script: Organize data on structural plasticity
  Input: preprocessed file from mESP project
  Output: .RDS file (temp) with aggregated data and eff sizes
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# upload data
mesp <- readRDS("~/surfdrive/Work/PhD/mESP/data/temp/structural_plasticity_complete.RDS")

exp <- readRDS(paste0(temp, "info_experiments.RDS"))

# this is only for males, you will have to do it also for females
# only total hippocampus kept
double_outcomes <- c(paste0("24129488_1_morph_", 2:6), 
                     paste0("27604299_3_morph_", 5:6), # might also be 2:6
                     paste0("27604299_4_morph_", 5:6), 
                     paste0("22371048_2_morph_", 17:19)
)
'
double check: https://doi.org/10.1080/10253890.2016.1224842
27604299
this paper reports both hippocampus dorsal and the hippocampus devided in CA1, CA2 and CA3
is it the same data?

double check branching of exp 20848608 (branching)
i dont understand if the data is double and for some reason the values are off,
or if they are actually different data (then the other_life_events is wrong)

double check https://doi.org/10.1523/JNEUROSCI.0247-10.2010
are "length dendrites" and "length primary dendrites" actually different things?
'



# Prepare meta data -------------------------------------------------------
mesp_ft <- mesp %>% 
  
  # keep only relevant
  filter(exp_id %in% exp$exp_id,
         ba_grouped == "hippocampus", 
         part_cell != "basal", 
         out_grouped != "complexity",
         !outcome_id %in% double_outcomes) %>% 
  select(
    # general vars
    exp_id, outcome_id, 
    # life events
    at_death, 
    # outcome
    outcome, out_grouped, product_measured, technique, 
    days_after_induction, 
    # brain area
    brain_area_publication, brain_area_hemisphere, ba_main,
    distance_cell, 
    # stats
    data_unit, ends_with("_c"), ends_with("_e")
    ) %>% 
  
  # clean
  rename(
    outcome_publication = outcome
  ) %>%
  
  # clean inconsistencies
  mutate(
    product_measured = case_when(
      product_measured == "mrna" ~ "rna", 
      product_measured == "not_applicable" & 
        out_grouped %in% c("brdu_cells", "dcx", "ki67") ~ "positive_cells",
      T ~ product_measured
    ), 
    
    ## keep all bdnf separately
    out_grouped = case_when(
      out_grouped == "bdnf" ~ paste(outcome_publication, product_measured, sep = "_"), 
      out_grouped == "brdu_cells" & days_after_induction <= 1 ~ paste("brdu", "short", sep ="_"),
      out_grouped == "brdu_cells" & days_after_induction > 1 ~ paste("brdu", "long", sep = "_"),
      T ~ out_grouped),
    
    
    # column to later cluster eff sizes > 1 value per outcome per experiment
    aggr = paste0(exp_id, out_grouped, product_measured)
  )

# put together with exp meta-data
mesp_cleaned <- exp %>% 
  right_join(mesp_ft, by = "exp_id") %>% 
  arrange(aggr)


# Calculate effect estimates -----------------------------------------------
## Specify rhos dependent of which eff sizes you are merging (my_rho)
rho_df <- mesp_cleaned %>% 
  mutate(
    aggr = paste0(exp_id, out_grouped)
  ) %>% 
  group_by(aggr) %>% 
  summarize(
    m_b = paste(unique(ba_main), collapse = ";"),
    merged_ba = length(paste0(unique(ba_main))), 
    merged_out = length(paste0(unique(outcome_publication))), 
    .groups = "drop"
  ) %>% 
  mutate(
    my_rho = case_when(
      merged_ba == 1 & merged_out == 1 ~ 0,
      merged_ba == 1 & merged_out > 1 ~ 0.75,
      merged_ba > 1 & merged_out == 1 ~ 0.25, 
      merged_ba > 1 & merged_out > 1 ~ 0.5, 
      T ~ 100
    )
  ) %>% 
#  select(aggr, my_rho) %>% 
  arrange(aggr)

# calculate effect sizes 
## hedges g
mesp_cleaned <- escalc("SMDH",
                  m1i = mean_e, sd1i = sd_e, n1i = n_e,
                  m2i = mean_c, sd2i = sd_c, n2i = n_c,
                  data = mesp_cleaned, var.names = c("yi_g", "vi_g"))


# aggregate outcomes (subparts of same area)
mesp_aggr <- aggregate(mesp_cleaned, cluster = aggr, rho = rho_df$my_rho)


# Save data ---------------------------------------------------------------

saveRDS(mesp_aggr, paste0(temp, "mesp_aggr.RDS"))
