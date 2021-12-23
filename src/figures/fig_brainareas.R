'
  Script: Visualizations frequency brain area
  Input: neurobiological data
  Output: Figure 3C paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'

ba_order <- c(dat_full %>% 
                filter(!is.na(ba_grouped), !str_detect(ba_grouped, "other")) %>% 
                group_by(ba_grouped) %>% count() %>% arrange(-n) %>% 
                pull(ba_grouped), "other_cortical", 
              "other_areas") %>% str_replace_all("_", " ")

g_freq_ba <- dat_full %>%
  filter(!is.na(ba_grouped)) %>%
  
  group_by(ba_grouped) %>% 
  summarize(
    n = length(unique(outcome_id)),
    .groups = "drop"
  ) %>% 
  mutate(
    tot_n = nrow(dat_full), 
    unique_id = n/tot_n*100, 
    ba_grouped = str_replace_all(ba_grouped, "_", " "), 
    ba_grouped = factor(ba_grouped, levels = rev(ba_order))
  ) %>%
  ggplot(aes(ba_grouped, unique_id)) + 
  geom_segment(aes(x=ba_grouped ,xend=ba_grouped, 
                   y=0, yend=unique_id), color="grey") +
  geom_point(size = 2) +
  coord_flip() + 
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40), 
                     labels = c("0%", "10%", "20%", "30%", "40%")) + 
  # scale_y_discrete(limits = "rev") + 
  labs(
    y = "% comparisons",
    x = "Brain areas", 
    title = "Brain areas"
  ) + 
  my_theme
