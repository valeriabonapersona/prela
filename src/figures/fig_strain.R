'
  Script: Visualizations frequency strain
  Input: output of frequency_df.R which merges mab and neurobiology data
  Output: Figure 3A paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'


g_strain <- freq_data %>% 
  mutate(strain = str_replace_all(strain, "_", " ")) %>%
  ggplot(aes(factor(strain,
                    levels = c("c57bl6", "balbc", "swiss webster", "cd1", 
                               "wistar", "sprague dawley", "long evans", "lister hooded",
                               "long evans hooded", "not specified","other")), fill = id)) +
  labs(y = expression("N"["comparisons"]), 
       x = "Strain", 
       title = "Species"
  ) + 
  my_hist + theme_hist + 
  facet_grid(~fct_rev(species), scales = "free_x") + 
  scale_fill_manual(values = rep("white", n_distinct(freq_data$id))) 

