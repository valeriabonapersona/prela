'
  Script: General frequency info for paper
  Input: processed files of all meta-analyses
  Output: frequencies
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")
source("src/upload_data.R")
source("src/upload_mab.R")

# General info ------------------------------------------------------------
# dates publications
## for behavior
min(mab$year)
max(mab$year)
## for neurobiology
min(exp$year)
max(exp$year)

# percentage species exp
get_freq <- function(data, var, id_var) {
  data %>% 
    select(c(id_var, var)) %>% 
    unique() %>%
    group_by_at(vars(all_of(var))) %>%
    summarize(
      across(id_var, n_distinct), 
      .groups = "drop"
    )
}

# species
## for behavior
get_freq(mab, "species", "exp")
## for neurobio
get_freq(dat_full, "species", "exp_id")



## for behavior
get_freq(mab %>% filter(strain == "wistar"), "strainGrouped", "exp") %>% pull(exp) / n_distinct(mab[mab$species == "rat",]$exp)
## for neurobio
get_freq(dat_full %>% filter(strain == "wistar"), "strain", "exp_id") %>% 
  pull(exp_id) / n_distinct(dat_full[dat_full$species == "rat",]$exp_id)

# ELA model
get_freq(mab %>% filter(model == "M"), "model", "exp") %>% pull(exp) /n_distinct(mab$exp)
get_freq(dat_full %>% filter(model == "maternal_separation"), "model", "exp_id") %>% pull(exp_id) /n_distinct(dat_full$exp_id)

# other info
## neurobio
n_distinct(dat_full[dat_full$cross_fostering == "yes",]$exp_id)/n_distinct(dat_full$exp_id)
n_distinct(dat_full[dat_full$litter_size != "not_specified",]$exp_id)/n_distinct(dat_full$exp_id)

# maternal separation
mab[mab$model == "M",] %>% mutate(time_end = case_when(mTimeEnd < 8 ~ "first", mTimeEnd < 15 ~ "second", T ~"third")) %>% group_by(time_end) %>% summarize(length(unique(exp)))

dat_full %>% 
  filter(model == "maternal_separation") %>%
  select(exp_id, model_postdays) %>% 
  separate(model_postdays, sep = "-", into = c("start", "end")) %>% 
  mutate(
    end = as.numeric(end),
    end = case_when(
      end < 8 ~ "first", 
      end < 15 ~ "second", 
      T ~ "third"
    )) %>% 
  unique() %>%
  group_by(end) %>% 
  summarize(length(unique(exp_id)))

## separation length
dat_full %>% 
  filter(model == "maternal_separation") %>%
  mutate(
    separation_hours = ifelse(separation_hours == "5_6", 5.5, as.numeric(as.character(separation_hours)))
  ) %>%
  summarize(median(separation_hours))

## predictability separation
get_freq(dat_full %>% filter(model == "maternal_separation"), "separation_repetition", "exp_id") %>% 
  pull(exp_id) /n_distinct(dat_full[dat_full$model == "maternal_separation",]$exp_id)

# light phase
get_freq(mab %>% filter(model == "M"), "mLDGrouped", "exp")
get_freq(dat_full %>% filter(model == "maternal_separation"), "separation_light_phase", "exp_id")

# age
max(mab$ageWeekNum, na.rm = T)
mab %>% filter(is.na(ageWeekNum)) %>% summarize(n_exp = length(unique(id))) # not reported
dat_full %>% filter(age_weeks == "not_specified") %>% summarize(n_exp = length(unique(id)))

## hippocampus
get_freq(dat_full %>% filter(ba_grouped == "hippocampal_region"), "ba_grouped", "outcome_id") %>% 
  pull(outcome_id) / n_distinct(dat_full$outcome_id)

dat_full %>% 
  filter(ba_grouped == "hippocampal_region") %>% 
  filter(ba_location == "") %>% 
  summarize(n_exp = length(unique(exp_id))) %>% 
  pull(n_exp) / n_distinct(dat_full[dat_full$ba_grouped == "hippocampal_region",]$exp_id)


## noradrenaline
dat_full %>% filter(out_grouped == "NE") %>% summarize(n_id = length(unique(id)), n_exp = length(unique(exp_id)))

dat_full %>% 
  filter(species == "rat", sex == "male", ba_grouped == "hippocampal_region", 
         model == "maternal_separation") %>% 
  summarize(n_exp = length(unique(exp_id))) / n_distinct(dat_full$exp_id)
