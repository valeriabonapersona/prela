'
  Script: Univariate analysis on g to summarize each outcome
  Input: processed data as uploaded with upload_data.R
  Output: ready dataframe with meta-analytic summ stats for visualization
  
  Author: v.bonapersona-2@umcutrecht.nl

'

limbic_overview_df <- limbic %>%
  filter(sex == "male") %>%
  filter(!out_grouped %in% c("complexity", "arc", "egr_variants", 
                             "n_primary_dendrites", "n_basal_dendrites", 
                             "other_func_outcome", "fosb_variants", "fos"))

limbic_meta_df <- data.frame()

# multiple ba
for (out in unique(limbic_overview_df$out_grouped)) {
  df <- limbic_overview_df %>% filter(out_grouped == out)
  
  if (n_distinct(df$ba_grouped) ==1 & nrow(df) > 2) {
    mod <- rma.mv(yi, vi, data = df, random = ~1|exp_id)
    res <- mod_to_df(mod) %>% 
      mutate(out_grouped = out, ba_grouped = unique(df$ba_grouped)) %>%
      left_join(get_n(df, "ba_grouped"), by = "ba_grouped")
  } else if (n_distinct(df$ba_grouped) > 1) {
    mod <- my_rma(my_dat = df, abs = F, var = "ba_grouped", intercept = F)
    res <- cbind(data.frame(out_grouped = out), mv_to_df(df, "ba_grouped", mod))
  }
  limbic_meta_df <- bind_rows(res, limbic_meta_df)  
}

limbic_meta_df <- limbic_meta_df %>% 
  left_join(limbic_overview_df %>% select(out_grouped, ba_grouped, domain), by = c("out_grouped", "ba_grouped"))

rm(limbic_overview_df)
