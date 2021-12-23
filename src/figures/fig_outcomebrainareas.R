'
  Script: Visualizations frequency outcome by brain area
  Input: neurobiological data
  Output: Figure 3C paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'


ba_order <- dat_full %>% 
                filter(!is.na(ba_grouped), !str_detect(ba_grouped, "other")) %>% 
                group_by(ba_grouped) %>% count() %>% arrange(-n) %>% 
                pull(ba_grouped) %>% str_replace_all("_", " ")

g_out_ba <- dat_full %>% 
  filter(!is.na(ba_grouped), !str_detect(ba_grouped, "other")) %>% 
  mutate(
    ba_grouped = str_replace_all(ba_grouped, "_", " "), 
    ba_grouped = factor(ba_grouped, levels = rev(ba_order)),
    domain = case_when(
      domain == "bdnf" ~ "BDNF", 
      str_detect(domain, "gaba_glu") ~ "GABA & glutamate", 
      domain == "da_related" ~ "DA-related", 
      domain == "ser_related" ~ "5HT-related", 
      domain == "ne_related" ~ "NE-related", 
      domain == "mono_enzymes" ~ "mono enzymes",
      domain %in% c("c_fos", "other_ieg") ~ "IEG", 
      T ~ str_replace_all(domain, "_", " ")
    ), 
    domain = factor(domain, 
                    levels = c("morphology", "BDNF", "IEG",  "neurogenesis", 
                               "5HT-related", "DA-related", "NE-related", 
                               "receptors and transporters","mono enzymes",
                               "GABA & glutamate"))# following structure figure 2
  ) %>%
  group_by(ba_grouped, domain) %>% 
  summarize(n = length(unique(exp_id)), .groups = "drop")  %>% 
  ggplot(aes(domain, ba_grouped, fill = n)) + 
  geom_tile() + 
  my_theme + 
  scale_fill_viridis() + 
  labs(x="Outcomes", y = "Brain areas", fill = expression("N"["exp"]), 
       title = "Frequency outcomes across brain areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
