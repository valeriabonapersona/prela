'
  Script: Visualizations overview limbic system
  Input: processed limbic meta-analysis data from limbic_overview_analysis.R
  Output: Figure 4C paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'

source("src/analyses/limbic_overview_analysis.R")

g_limbic_overview <- limbic_meta_df %>% 
  mutate(
    dir = ifelse(estimate < 0, -1, 1),
    estimate = case_when(
      estimate > 2 ~ 2, 
      estimate < -2 ~ -2, 
      T ~ estimate
    ), 
    domain = case_when(
      domain == "bdnf" ~ "BDNF", 
      str_detect(domain, "gaba_glu") ~ "GABA & glutamate", 
      domain == "da_related" ~ "DA-related", 
      domain == "ser_related" ~ "5HT-related", 
      domain == "ne_related" ~ "NE", 
      domain == "mono_enzymes" ~ "mono \n enz",
      domain %in% c("c_fos", "other_ieg") ~ "IEG", 
      T ~ str_replace_all(domain, "_", " ")
    ), 
    domain = factor(domain, 
                    levels = c("morphology", "BDNF", "IEG",  "neurogenesis", 
                               "5HT-related", "DA-related", "NE", 
                               "receptors and transporters","mono \n enz",
                               "GABA & glutamate"))
  ) %>%
  ggplot(aes(str_replace_all(ba_grouped, "_", "\n "),
             str_replace_all(out_grouped, "_", " "),
             size = n_outcome_id, fill = estimate)) + 
  my_theme + 
  geom_point(shape = 21) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0) +
  facet_grid(domain~., scales = "free", space = "free", switch = "both") + 
  labs(x = "Limbic areas", y = "outcomes", fill = expression(italic("g")), 
       size = expression(N[comp])) + 
  theme(legend.position = "top") + 
  theme(panel.spacing = unit(0.75, "lines")) +
  theme(strip.background = element_rect(color = "white")) +       
  theme(strip.text.y = element_text(size = 6, colour = "black",face = "bold")) 
