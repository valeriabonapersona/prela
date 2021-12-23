'
  Script: Visualizations anxiety-prone phenotype
  Input: output of results_anxiety_prone_df.R
  Output: Figure 5A paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'

source("src/figures_prep/results_anxietyprone_df.R")

facet_order <- c("behavior", "plasma", "IEG", "BDNF")
out_order <- c("anxiety", "non stressful \n learning", "stressful \n learning", 
               "plasma epi \n at rest",
               "cfos \n at rest", "cfos \n after stress", "BDNF \n at rest", 
               "BDNF \n after stress")
g_anxiety_prone <- anxietyprone_df %>% 
  mutate(
    my_facet = case_when(
      out_grouped %in% c("anxiety", "stressful_learning", "nonstressful_learning") ~ "behavior", 
      str_detect(out_grouped, "cfos") ~ "IEG", 
      str_detect(out_grouped, "plasma") ~ "plasma",
      str_detect(out_grouped, "bdnf") ~ "BDNF",
      
    ),
    out_grouped = case_when(
      out_grouped == "plasma_epi" ~ "plasma epi \n at rest", 
      out_grouped == "bdnf" & at_death_grouped == "rest" ~ "BDNF \n at rest",
      out_grouped == "bdnf" & at_death_grouped == "not_rest" ~ "BDNF \n after stress",
      out_grouped == "nonstressful_learning" ~ "non stressful \n learning",
      out_grouped == "stressful_learning" ~ "stressful \n learning",
      out_grouped == "cfos_rest" ~ "cfos \n at rest",
      out_grouped == "cfos_not_rest" ~ "cfos \n after stress", 
      T ~ out_grouped
    ),
    at_death_grouped = ifelse(at_death_grouped == "rest", "at rest", "(after) stress")
    ) %>%
  ggplot(aes(factor(out_grouped, levels = out_order), estimate, fill = at_death_grouped)) + 
  geom_bar(stat = "identity", color = "grey55") + 
  ylim(c(-0.9, 1.25)) + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, color = "black") +
  labs(y = expression(italic("g[CI]")), 
       x = "Outcomes", fill = "Acute situation") +
  my_theme + 
  facet_grid(~factor(my_facet, levels = facet_order), scales = "free_x", space = "free_x") + 
  scale_fill_manual(values = c("grey75", "white")) + 
  geom_text(aes(label = n_outcome_id), y= -0.85) + 
  geom_text(aes(label = sig), y= 1.2) +
  theme(legend.position = "bottom")
