'
  Script: Analysis plasma epinephrine
  Input: raw data
  Output: results in df
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Upload data -------------------------------------------------
plasma_epi <- readRDS(paste0(final, "prela_plasma_epi.RDS"))

# trauma presence
plasma_epi <- plasma_epi %>% 
  mutate(
    trauma_presence = case_when(
      str_detect(other_life_experience, "surgery|chronic stress|anesth") ~ "yes",
      T ~ "no"
    )
  )

# Epinephrine -------------------------------------------------------------
plasma_epi<- escalc("SMDH",
                    m1i = mean_e, sd1i = sd_e, n1i = n_e,
                    m2i = mean_c, sd2i = sd_c, n2i = n_c,
                    data = plasma_epi)

mod_epi <- rma.mv(yi, vi, 
                  #   ultilevel not necessary because all different papers
                  data = plasma_epi %>% filter(sex=="male"))


res_epi <- mod_to_df(mod_epi) %>%
  cbind(get_n(plasma_epi, "outcome")) %>% 
  rename(domain = outcome) %>% 
  relocate(domain)


# Epi trauma --------------------------------------------------------------
mod_epi_trauma <- rma.mv(yi, vi, 
                  #   ultilevel not necessary because all different papers
                  mods = ~trauma_presence,
                  data = plasma_epi %>% filter(sex=="male"))


res_epi_trauma <- mv_to_df(plasma_epi, "trauma_presence", mod_epi_trauma)
wald_epi_trauma <- res_epi_trauma %>% 
  rename(wald = trauma_presence) %>%
  filter(wald != "intrcpt") %>% 
  mutate(wald = ifelse(wald == "yes", "epi_trauma")) %>%
  select(-ends_with("_id"))

## data for visual
mod_epi_trauma_nointer <- rma.mv(yi, vi, 
                         #   ultilevel not necessary because all different papers
                         mods = ~trauma_presence -1,
                         data = plasma_epi %>% filter(sex=="male"))


res_epi_trauma <- mv_to_df(plasma_epi, "trauma_presence", mod_epi_trauma_nointer)
res_epi_trauma_hit <- res_epi_trauma %>% 
 mutate(domain = "plasma_epi")

rm(mod_epi, mod_epi_trauma,mod_epi_trauma_nointer, res_epi_trauma)
