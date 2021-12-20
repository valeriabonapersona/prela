'
  Script: Analysis plasma epinephrine
  Input: raw data
  Output: results in df
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Prepare data -------------------------------------------------
cfos_amyg <- dat %>% 
  filter(ba_grouped == "amygdala", 
         sex == "male", 
         domain == "c_fos"
         ) %>% 
  group_by(domain, ba_grouped, at_death_grouped, sex) %>% 
  summarize(n = length(unique(exp_id))) %>%
  filter(n>2) %>% 
  left_join(dat, 
            by = c("ba_grouped", "sex", "domain", "at_death_grouped")) %>% 
  filter(!domain %in% "5HIAA/5HT")

# cfos main -------------------------------------------------------------
mod_cfos_amyg <- rma.mv(
  yi, vi, 
  random = list(~1 | outcome_id, ~1 | exp_id),
  mods = ~at_death_grouped-1,
  data = cfos_amyg
)

# visualization of amygdala results
res_cfos_amyg <- mv_to_df(cfos_amyg, "at_death_grouped", mod_cfos_amyg)

res_cfos_amyg <- res_cfos_amyg %>% 
  rename(domain = at_death_grouped) %>% 
  mutate(domain = paste("cfos", domain, sep = "_"))

rm(mod_cfos_amyg)


# cfos wald ---------------------------------------------------------------
## perform two separate MA 
res1 <- rma.mv(yi, vi, data=cfos_amyg, subset=at_death_grouped=="rest")
res2 <- rma.mv(yi, vi, data=cfos_amyg, subset=at_death_grouped=="not_rest")

# put in same df
dat.comp <- data.frame(
  estimate = c(coef(res1), coef(res2)), 
  stderror = c(res1$se, res2$se),
  at_death_grouped = c("rest","not_rest"), 
  tau2 = round(c(res1$tau2, res2$tau2),3))

# fixed effect model to compare the two 
mod_wald <- rma(estimate, sei=stderror, mods = ~ at_death_grouped, method="FE", data=dat.comp, digits=3)

# get results ready for publ
wald_cfos_amyg <- mv_to_df(cfos_amyg, "at_death_grouped", mod_wald)

wald_cfos_amyg <- wald_cfos_amyg %>% 
  rename(wald = at_death_grouped) %>%
  filter(wald != "intrcpt") %>% 
  mutate(wald = ifelse(wald == "rest", "cfos_at_death")) %>%
  select(-ends_with("_id"))

rm(dat.comp, res1, res2, cfos_amyg)
