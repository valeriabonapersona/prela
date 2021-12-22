'
  Script: Visualizations frequency model
  Input: output of frequency_df.R which merges mab and neurobiology data
  Output: Figure 3B paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'

g_model <- freq_data %>% 
  mutate(model = str_replace_all(model, "_", " ")) %>%
  ggplot(aes(factor(model, levels = c("maternal separation", "maternal deprivation", "isolation", "limited nesting", "licking grooming")), fill = id)) + 
  labs(
    y = expression("N"["comparisons"]), 
    x = "ELA model", 
    title = "ELA model"
  ) + 
  my_hist + theme_hist + 
  scale_fill_manual(values = rep("white", n_distinct(freq_data$id))) 
