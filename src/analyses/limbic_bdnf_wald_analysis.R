'
  Script: Wald subgroup analysis acute situation BDNF
  Input: processed data (already aggregated if necessary)
  Output: results in df
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Prepare data -------------------------------------------------
limbic <- dat %>% 
  filter(sex == "male", 
         ba_grouped %in% c("hippocampal_region", "prefrontal_cortex"), 
         # exclude morphology and neurogenesis because not likely to change with acute stress
         out_grouped == "bdnf"
  )


# bdnf limbic acute wald ---------------------------------------------------------------
limbic_df <- limbic %>% filter(ba_grouped == "hippocampal_region")
## hippocampus 
res1 <- rma.mv(yi, vi, data=limbic_df, subset=at_death_grouped=="rest")
res2 <- rma.mv(yi, vi, data=limbic_df, subset=at_death_grouped=="not_rest")

# put in same df
dat.comp <- data.frame(
  estimate = c(coef(res1), coef(res2)), 
  stderror = c(res1$se, res2$se),
  at_death_grouped = c("rest","not_rest"), 
  tau2 = round(c(res1$tau2, res2$tau2),3))

# fixed effect model to compare the two 
mod_wald <- rma(estimate, sei=stderror, mods = ~ at_death_grouped, method="FE", data=dat.comp, digits=3)

# get results ready for publ
wald_bdnf <- mv_to_df(limbic_df, "at_death_grouped", mod_wald)

wald_bdnf_hip <- wald_bdnf %>% 
  rename(wald = at_death_grouped) %>%
  filter(wald != "intrcpt") %>% 
  mutate(wald = ifelse(wald == "rest", "bdnf_hip")) %>%
  select(-ends_with("_id"))

## pfc
limbic_df <- limbic %>% filter(ba_grouped == "prefrontal_cortex")
## hippocampus 
res1 <- rma.mv(yi, vi, data=limbic_df, subset=at_death_grouped=="rest")
res2 <- rma.mv(yi, vi, data=limbic_df, subset=at_death_grouped=="not_rest")

# put in same df
dat.comp <- data.frame(
  estimate = c(coef(res1), coef(res2)), 
  stderror = c(res1$se, res2$se),
  at_death_grouped = c("rest","not_rest"), 
  tau2 = round(c(res1$tau2, res2$tau2),3))

# fixed effect model to compare the two 
mod_wald <- rma(estimate, sei=stderror, mods = ~ at_death_grouped, method="FE", data=dat.comp, digits=3)

# get results ready for publ
wald_bdnf <- mv_to_df(limbic_df, "at_death_grouped", mod_wald)

wald_bdnf_pfc <- wald_bdnf %>% 
  rename(wald = at_death_grouped) %>%
  filter(wald != "intrcpt") %>% 
  mutate(wald = ifelse(wald == "rest", "bdnf_pfc")) %>%
  select(-ends_with("_id"))

wald_bdnf_limbic <- bind_rows(wald_bdnf_hip, wald_bdnf_pfc) %>% 
  mutate_if(is.numeric, round, digits=3)

rm(dat.comp, res1, res2, cfos_amyg)
