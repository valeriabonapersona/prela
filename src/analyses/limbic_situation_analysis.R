'
  Script: Analysis limbic outcomes with situation
  Input: processed data (already aggregated if necessary)
  Output: results in df
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Prepare data -------------------------------------------------
limbic <- dat %>% 
  filter(sex == "male", 
         ba_grouped %in% c("hippocampal_region", "prefrontal_cortex"), 
         # exclude morphology and neurogenesis because not likely to change with acute stress
         !domain %in% c("morphology", "neurogenesis"),
         !str_detect(out_grouped, "basal|c_fos")
         )

suff_n <- limbic %>%
  
  group_by(ba_grouped, domain, out_grouped, at_death_grouped) %>% 
  
  # count how much evidence
  summarize(across(ends_with("id"), n_distinct), 
            .groups = "drop") %>%
  rename(n_id = id, n_exp_id = exp_id, n_out_id = outcome_id) 

# keep outcomes only if studied in >3 publ
limbic_df <- limbic %>% 
  left_join(suff_n) %>% 
  filter(n_id > 2)


# Analysis ----------------------------------------------------------------
mod_limbic_acute <- rma.mv(
  yi, vi, 
  random = list(~1 | outcome_id, ~1 | exp_id),
  mods = ~ba_grouped:out_grouped:at_death_grouped-1,
  data = limbic_df 
)


# organize data
res_limbic_acute <- mod_to_df(mod_limbic_acute) %>% 
  rownames_to_column("comparison") %>% 
  mutate(comparison = str_remove_all(comparison, "ba_grouped") %>% 
           str_remove_all("out_grouped") %>% 
           str_remove_all("at_death_grouped")) %>%
  separate(comparison, into = c("ba_grouped", "out_grouped", "at_death_grouped"), sep = ":") %>% 
  left_join(suff_n) %>%
  mutate_if(is.numeric, round, digits=3) %>% 
  arrange(ba_grouped, out_grouped)



# Save --------------------------------------------------------------------
write.csv(res_limbic_acute, "results/output/res_situation_limbic.csv")


# Cleaning up -------------------------------------------------------------
rm(limbic_df, suff_n, mod_limbic_acute)