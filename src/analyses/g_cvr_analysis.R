'
  Script: Efficacy and stability across elements of the experimental design
  Input: 
  Output: 
  
  Author: v.bonapersona-2@umcutrecht.nl

'
dat_cvr <- dat %>%
  select(-c(yi, vi)) %>% 
  rename(yi = yi_cvr, vi = vi_cvr)


vars <- c("species", "sex", "model")

g_ls <- list()
cvr_ls <- list()
for(i in vars) {
  print(i)
  g_mod <- my_rma(my_dat = dat, abs = T, var = i, intercept = F)
  g_ls[[i]] <- mv_to_df(dat, i, g_mod)
  
  cvr_mod <- my_rma(my_dat = dat_cvr, abs = F, var = i, intercept = F)
  cvr_ls[[i]] <- mv_to_df(dat_cvr, i, cvr_mod)
}

res_g_cvr <- bind_rows(g_ls) %>% 
  mutate(eff_size = "g") %>%
  bind_rows(bind_rows(cvr_ls) %>% mutate(eff_size = "cvr")) %>% 
  mutate(
    out_grouped = paste0(species, sex, model) %>% str_remove_all("NA"), 
    out_grouped = case_when(
      out_grouped == "rat" ~ "rats", 
      out_grouped == "male" ~ "males", 
      out_grouped == "female" ~ "females", 
      T ~ out_grouped
    ),
    domain = case_when(
      out_grouped %in% c("mice", "rats") ~ "species", 
      out_grouped %in% c("males", "females") ~ "sex", 
      T ~ "model"
    )
  ) %>% 
  select(-c(species, sex, model))
 

# ## for numeric output
# # differences in species
# my_rma(my_dat = dat, abs = T, var = "species", intercept = F)
# my_rma(my_dat = dat, abs = T, var = "species:at_death_grouped", intercept = F)
# my_rma(my_dat = dat_cvr, abs = T, var = "species")
# 
# # differences in sex
# my_rma(my_dat = dat, abs = T, var = "sex")
# my_rma(my_dat = dat_cvr, abs = T, var = "sex")
# 
# 
# # ELA model
# my_rma(my_dat = dat, abs = T, var = "model", intercept = F)
# 
# dat_lg <- dat %>% mutate(model_grouped = ifelse(model == "licking_grooming", "licking_grooming", "other"))
# my_rma(my_dat = dat_lg, abs = T, var = "model_grouped")
# 
# dat_lg_cvr <- dat_cvr %>% mutate(model_grouped = ifelse(model == "licking_grooming", "licking_grooming", "other"))
# my_rma(my_dat = dat_lg_cvr, abs = T, var = "model_grouped")

