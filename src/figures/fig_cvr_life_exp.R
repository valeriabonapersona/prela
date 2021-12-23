'
  Script: Visualization cvr across life experience
  Input: results from cvr analysis across life experience (cvr_life_exp_analysis.R)
  Output: figure 4b
  
  Author: v.bonapersona-2@umcutrecht.nl

'

source("src/analyses/cvr_life_exp_analysis.R")
order_life_exp <- c("none", "non stressful \n behavior", "+1 hit", "+2 hit", "+3 hit")


g_cvr_life_exp <- res_cvr_life_exp %>% 
  mutate(life_exp = case_when(
    life_exp == "naive" ~ "none",
    life_exp == "non_stressful" ~ "non stressful \n behavior",
    T  ~ life_exp)) %>%
  ggplot(aes(factor(life_exp, levels = order_life_exp), estimate)) + 
  ylim(c(-0.1, 0.65)) + 
  geom_bar(stat = "identity", fill = "white", color = "grey55") + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, color = "black") +
  labs(y = "CVR", 
       x = "Life experiences") +
  my_theme + 
  geom_text(aes(label = n_outcome_id), y= -0.05) + 
  geom_text(aes(label = sig), y= 0.6)

g_cvr_life_exp

