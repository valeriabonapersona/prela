'
  Script: Visualizations hits across outcomes
  Input: output of results_hits_df.R
  Output: Figure 5B paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'

source("src/figures_prep/results_hits_df.R")

facet_order_hit <- c("behavior", "plasma", "whole-brain IEG", "hip morphology")

g_hits <- res_hits %>% 
  filter(!str_detect(domain, "int")) %>%
  mutate(
    my_out = paste(domain, trauma_presence), 
    my_facet = case_when(
      str_detect(domain, "cfos") ~ "whole-brain IEG",
      str_detect(domain, "learning") | domain == "anxiety"~ "behavior", 
      str_detect(domain, "plasma") ~ "plasma",
      domain == "morphology" ~ "hip morphology",
      T ~ domain
    ), 
    my_fill = case_when(
      domain %in% c("plasma_epi", "cfos \n at rest", 
                    "nonstressful_learning", "morphology") ~ "at rest", 
      T ~ "(after) stress"
    ), 
    my_out = str_replace_all(my_out, "_", "\n") %>%
      str_replace_all(" no", "") %>% 
      str_replace_all(" yes", "\n +hits") %>% 
      str_replace_all("  after stress", "\n after stress") %>% 
      str_replace_all("morphology", "morph") %>% 
      str_replace_all("nonstressful", "non stressful") %>%
      str_replace_all("stressful", "stress.")
  ) %>%
  ggplot(aes(my_out, estimate, fill = my_fill)) + 
  geom_bar(stat = "identity", color = "grey55") + 
  geom_hline(yintercept = 0) + 
  ylim(c(-1.95, 1.7)) + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, color = "black") +
  labs(y = expression(italic("g[CI]")), 
       x = "Outcomes", fill = "Acute situation") +
  my_theme + 
  facet_grid(~factor(my_facet, facet_order_hit), scales = "free_x", space = "free_x") + 
  scale_fill_manual(values = c("grey75", "white")) + 
  geom_text(aes(label = n_outcome_id), y= -1.9) + 
  geom_text(aes(label = sig), y= 1.6) +
  theme(legend.position = "bottom")

