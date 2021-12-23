'
  Script: Analysis limbic outcomes with situation
  Input: processed data (already aggregated if necessary)
  Output: results in df
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Prepare data -------------------------------------------------
limbic_ft <- limbic %>% 
  filter(sex == "male", 
         ba_grouped %in% c("hippocampal_region", "prefrontal_cortex"), 
         !str_detect(out_grouped, "basal|c_fos")
  )



# Analysis ----------------------------------------------------------------
# just look at test of moderators
# for each ba separately
# for each out separately

mod_to_Qm <- function(mod, var = "var") {
  # mod is a model from rma or rma.mv
  # var is a string to keep track of what you are doing
  data.frame(
    var = var, 
    q_df = mod$QMdf[[1]], 
    q_m = mod$QM, 
    q_pval = mod$QMp
  ) %>% 
    mutate(
      sig = case_when(
        q_pval < 0.001 ~ "***", 
        q_pval < 0.01 ~ "**", 
        q_pval < 0.05 ~ "*", 
        T ~ ""
      )
    )
}

get_mod_test <- function(df, my_mods, var) {
  
  mod_tmp <- rma.mv(
    yi, vi, 
    random = list(~1 | outcome_id, ~1 | exp_id),
    mods = as.formula(my_mods),
    data = df
  )
  
  mod_to_Qm(mod_tmp, var)
}
res_hit <- data.frame()
res_int <- data.frame()
for (ba in c("hippocampal_region", "prefrontal_cortex")) {
  
  for (out in unique(limbic_ft$out_grouped)) {
    
    print(paste(ba, out))
    
    # filter the data
    df <- limbic_ft %>% filter(ba_grouped == ba, out_grouped == out)
    
    if (nrow(df) > 2) {
      # trauma presence : check if sufficient
      hit_check <- df %>% group_by(trauma_presence) %>% count()
      
      if (nrow(hit_check) > 1 & min(hit_check$n) > 2) {
        hit <- get_mod_test(df, "~trauma_presence", out)
        res_hit <- bind_rows(res_hit, cbind(ba_grouped = ba, hit))
      } 
      
      # interaction with acute situation
      int_check <- df %>% group_by(trauma_presence, at_death_grouped) %>% count()
      
      if (nrow(int_check) > 2 & min(hit_check$n) > 2) {
        int <- get_mod_test(df, "~trauma_presence:at_death_grouped", out)
        res_int <- bind_rows(res_int, cbind(ba_grouped = ba, int))
      } 
    }
  }
}

