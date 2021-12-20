'
  Script: Analysis plasma epinephrine
  Input: raw data
  Output: results in df
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Upload data -------------------------------------------------
plasma_epi <- readRDS(paste0(final, "prela_plasma_epi.RDS"))

# Epinephrine -------------------------------------------------------------
plasma_epi<- escalc("SMDH",
                    m1i = mean_e, sd1i = sd_e, n1i = n_e,
                    m2i = mean_c, sd2i = sd_c, n2i = n_c,
                    data = plasma_epi)

mod_epi <- rma.mv(yi, vi, 
                  #   ultilevel not necessary because all different papers
                  data = plasma_epi %>% filter(sex=="male"))

res_epi <- mod_to_df(mod_epi) %>%
  cbind(get_n(plasma_epi, "outcome"))

rm(mod_epi)