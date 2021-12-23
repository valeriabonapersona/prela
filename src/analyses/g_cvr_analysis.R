'
  Script: Efficacy and stability across elements of the experimental design
  Input: 
  Output: 
  
  Author: v.bonapersona-2@umcutrecht.nl

'
dat_cvr <- dat %>%
  select(-c(yi, vi)) %>% 
  rename(yi = yi_cvr, vi = vi_cvr)

# differences in species
my_rma(my_dat = dat, abs = T, var = "species")
my_rma(my_dat = dat, abs = T, var = "species:at_death_grouped", intercept = F)
my_rma(my_dat = dat_cvr, abs = T, var = "species")

# differences in sex
my_rma(my_dat = dat, abs = T, var = "sex")
my_rma(my_dat = dat_cvr, abs = T, var = "sex")


# ELA model
my_rma(my_dat = dat, abs = T, var = "model", intercept = F)

dat_lg <- dat %>% mutate(model_grouped = ifelse(model == "licking_grooming", "licking_grooming", "other"))
my_rma(my_dat = dat_lg, abs = T, var = "model_grouped")

dat_lg_cvr <- dat_cvr %>% mutate(model_grouped = ifelse(model == "licking_grooming", "licking_grooming", "other"))
my_rma(my_dat = dat_lg_cvr, abs = T, var = "model_grouped")




dat_overall <- dat %>% 
  mutate(
    origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0), 
    major_life_events_num = ifelse(major_life_events == "yes", 1, 0), 
    behavior_num = ifelse(behavior == "stressful", 1, 0)
  ) %>% 
  rowwise() %>% 
  mutate(
    trauma_score = factor(sum(origin_num, major_life_events_num, behavior_num), levels = c("0", "1", "2", "3")),
    trauma_score = paste0("+", trauma_score)
  ) %>% 
  select(-c(ends_with("_num"))) %>%
  mutate(
    life_exp = case_when(
      trauma_score == "+0" & behavior %in% c("naive", "no_behavior") ~ "naive", 
      trauma_score == "+0" & behavior %in% c("non_stressful") ~ "non_stressful", 
      T ~ paste(trauma_score, "hit")
    )
  ) %>%
  select(sex, species, model, life_exp,  origin, major_life_events, behavior, housing_after_weaning, trauma_score, 
         out_grouped, outcome, ba_grouped, yi, vi)

vars_moderators <- c("species", "sex", "model")

lapply(vars_moderators, function(vars) get_rma_summ_stats(x = vars)) -> res_ls

res_df <- bind_rows(res_ls) %>% 
  rownames_to_column(var = "subgroup") %>% 
  rowwise() %>% 
  mutate(subgroup = str_remove(subgroup, group))

order_facets <- c("species", "sex", "model", #"brain areas", 
                  "life exp", "out grouped"
                  # "hits", "naive", "origin", "behavior", 
                  # "chronic\ntrauma", 
                  # "housing"
)

order_ylab <- c("rat", "mice", "male", "female", 
                "licking grooming", "limited nesting", "isolation", 
                "maternal separation", "maternal deprivation",
                "naive", "non stressful", "+1 hit", "+2 hit", "+3 hit")

g_g <- res_df %>% 
  mutate(
    subgroup = str_replace_all(subgroup, "_", " "),
    group = case_when(
      group == "ba_grouped" ~ "brain_areas",
      group == "housing_after_weaning" ~ "housing", 
      group == "major_life_events" ~ "chronic\ntrauma", 
      group == "trauma_score" ~ "hits",
      T ~ group),
    group = str_replace_all(group, "_", " ")
  ) %>%
  ggplot(aes(factor(subgroup, levels = rev(order_ylab)), estimate)) + 
  #  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, color = "grey") +
  geom_point() + 
  #  ylim(0.15, 1.3) + 
  facet_grid(factor(group, levels = order_facets)~., scales = "free", space = "free", switch = "both") + 
  coord_flip() + 
  labs(y = "abs(Hedge's g)",
       #y = "CVR", 
       x = "Potential moderators") +
  my_theme + 
  geom_text(aes(label = n), y= 0.2) + 
  #  geom_text(aes(label = n), y= -0.23) + 
  
  theme(panel.spacing = unit(0.75, "lines")) +
  theme(#panel.spacing.x = unit(3,"line"), 
    strip.background = element_rect(color = "white")) +       theme(strip.text.y = element_text(size = 6, colour = "black",face = "bold"))


```


```{r CVR analysis}

dat_overall <- dat %>% 
  select(sex, species, model, life_exp,  origin, major_life_events, behavior, housing_after_weaning, trauma_score, 
         out_grouped, outcome, ba_grouped, yi_cvr, vi_cvr) %>% 
  rename(yi = yi_cvr, vi = vi_cvr)

vars_moderators <- c("species", "sex", "model", "life_exp"
                     #"behavior", "trauma_score", "housing_after_weaning", 
                     #   "major_life_events", "origin", "out_grouped", 
                     #  "ba_grouped"
)

lapply(vars_moderators, function(vars) get_rma_summ_stats(x = vars, abs = F)) -> res_ls

res_df <- bind_rows(res_ls) %>% 
  rownames_to_column(var = "subgroup") %>% 
  rowwise() %>% 
  mutate(subgroup = str_remove(subgroup, group))

g_cvr <- res_df %>% 
  mutate(
    subgroup = str_replace_all(subgroup, "_", " "),
    group = case_when(
      group == "ba_grouped" ~ "brain_areas",
      group == "housing_after_weaning" ~ "housing", 
      group == "major_life_events" ~ "chronic\ntrauma", 
      group == "trauma_score" ~ "hits",
      T ~ group),
    group = str_replace_all(group, "_", " ")
  ) %>%
  #  ggplot(aes(reorder(subgroup, estimate), estimate)) + 
  ggplot(aes(factor(subgroup, levels = rev(order_ylab)), estimate)) + 
  #  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, color = "grey") +
  geom_point() + 
  #  ylim(0.15, 1.3) + 
  facet_grid(factor(group, levels = order_facets)~., scales = "free", space = "free", switch = "both") + 
  coord_flip() + 
  labs(#y = "abs(Hedge's g)",
    y = "CVR", 
    x = "Potential moderators") +
  my_theme + 
  geom_text(aes(label = n), y= -0.23) + 
  #  geom_text(aes(label = n), y= -0.23) + 
  
  theme(panel.spacing = unit(0.75, "lines")) +
  theme(#panel.spacing.x = unit(3,"line"), 
    strip.background = element_rect(color = "white")) +       theme(strip.text.y = element_text(size = 6, colour = "black",face = "bold"))


```

```{r g/CVR fig}
ggpubr::ggarrange(
  g_g, g_cvr,
  ncol = 2, labels = c("A", "B")
)
```
