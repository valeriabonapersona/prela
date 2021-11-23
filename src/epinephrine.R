'
  Script: Cleaning plasma proteins
  
  Author: Valeria Bonapersona
  
  Input: excel file with relational structure, manually coded
  Output: structural plasticity data to be analyzed
  
'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# upload data
temp_path <- "~/surfdrive/Work/PhD/mELA_materials/Eline/all_files_mELA_2020/5. Files/outcomes_mELA_VB_v3.xlsx"
dat_xl <- read_my_sheets(temp_path, c("publications","experiments", "out_plasma"))
plasma <- dat_xl[["out_plasma"]]

exp_prela <- readRDS("~/surfdrive/Work/PhD/prela/data/temp/info_experiments.RDS")

plasma_full <- plasma %>% 
  left_join(dat_xl$exp, by = "exp_id") %>% 
  rename(
    mean_e = estimate_e, 
    mean_c = estimate_c, 
    sem_e = deviation_e, 
    sem_c = deviation_c
  ) %>% 
  mutate(
    sem_c = ifelse(sem_c == "not_specified", sem_e, sem_c), 
    sem_c = as.numeric(sem_c),
    sd_c = sem_c * sqrt(n_c),
    sd_e = sem_e * sqrt(n_e)
  )

dat <- escalc("SMDH",
              m1i = mean_e, sd1i = sd_e, n1i = n_e,
              m2i = mean_c, sd2i = sd_c, n2i = n_c,
              data = plasma_full)

mod <- rma.mv(yi, vi, 
              #   random = ~ 1|id, ## not necessary because all independent
              data = dat[dat$outcome == "E",], mods = ~sex - 1)
forest(mod)


