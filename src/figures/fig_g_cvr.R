'
  Script: Visualizations comparison g and cvr
  Input: output of g_cvr_analysis
  Output: Figure 7 paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'

source("src/analyses/g_cvr_analysis.R")

order_effsize <- c("abs(g)", "cvr")
order_domain <- c("species", "sex", "model")
order_out <- c("mice", "rats", "males", "females", 
               "licking \ngrooming", "limited \nnesting", 
               "maternal \nseparation", 
               "maternal \ndeprivation", "isolation")

g_g_cvr <- res_g_cvr %>% 
  mutate(out_grouped = str_replace_all(out_grouped, "_", " \n"), 
         eff_size = ifelse(eff_size == "g", "abs(g)", eff_size)) %>%
  
  ggplot(aes(factor(out_grouped, rev(order_out)), estimate)) + 
  #  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, color = "grey") +
  geom_point() + 
  #  ylim(0.15, 1.3) + 
  facet_grid(factor(domain, order_domain)~factor(eff_size, order_effsize), scales = "free", 
             space = "free_y", switch = "both") + 
  coord_flip() + 
  labs(y = "",
       x = "Potential moderators") +
  my_theme + 
  geom_vline(xintercept = 0, color = "red") + 
  
  theme(panel.spacing = unit(0.75, "lines")) +
  theme(#panel.spacing.x = unit(3,"line"), 
    panel.background = element_rect(fill = "grey98"),
    strip.background = element_rect(color = "white")) +       
  theme(strip.text.y = element_text(size = 6, colour = "black",face = "bold"))

