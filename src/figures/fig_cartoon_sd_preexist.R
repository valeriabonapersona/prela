'
  Script: Visualizations cartoon ELA preexisting differences
  Input: 
  Output: Figure 4a paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'

# Visualization -----------------------------------------------------------

funcShaded <- function(x, sd = 1) {
  y <- dnorm(x, mean = 0, sd = sd)
  
  y[x > -sd & x < sd] <- NA
  return(y)
}

funcShaded_3 <- function(x, sd = 3) {
  y <- dnorm(x, mean = 0, sd = sd)
  
  y[x > -sd & x < sd] <- NA
  return(y)
}
# 
# g_cartoon_control <- ggplot(data.frame(x = c(-5, 5)), aes(x = x)) +
#   
#   # sd 1 
#   stat_function(fun = dnorm, geom = "area", args = list(0, 1), alpha = 0.7,
#                 fill = "white", colour = "black") + 
#   stat_function(fun=funcShaded, geom="area", fill="grey55", alpha=0.2) + 
#   geom_segment(x = c(-1, 1), xend = c(-1,1), y = 0, yend = 0.24) + 
# 
#   # arrows
#   geom_segment(aes(x = -1, y = 0.2, xend = -3, yend = 0.2),
#                arrow = arrow(length = unit(0.5, "cm"))) + 
#   geom_segment(aes(x = 1, y = 0.2, xend = 3, yend = 0.2),
#                arrow = arrow(length = unit(0.5, "cm"))) + 
#   
#   my_theme + 
#   scale_fill_grey(start = 0, end = 0.8) + 
#   scale_color_grey(start = 0, end = 0.8) + 
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(), 
#         legend.position = "none"
#   )


g_cartoon_ela <- ggplot(data.frame(x = c(-5, 5)), aes(x = x)) +

  # sd 1 
  stat_function(fun = dnorm, geom = "area", args = list(0, 1), 
                fill = "white", colour = "black") + 
  stat_function(fun=funcShaded, geom="area", fill="grey55") + 
  geom_segment(x = c(-1, 1), xend = c(-1,1), y = 0, yend = 0.24) + 
  
  # sd 3
  stat_function(fun = dnorm, geom = "area", args = list(0, 3), 
                fill = "white", colour = "black", alpha = 0.85) + 
  stat_function(fun=funcShaded_3, geom="area", fill="grey55", alpha = 0.85) + 
  geom_segment(x = c(-3, 3), xend = c(-3,3), y = 0, yend = 0.24) + 
  
  
  # arrows
  geom_segment(aes(x = -1, y = 0.2, xend = -3, yend = 0.2),
               arrow = arrow(length = unit(0.5, "cm"))) + 
  geom_segment(aes(x = 1, y = 0.2, xend = 3, yend = 0.2),
               arrow = arrow(length = unit(0.5, "cm"))) + 
  
  my_theme + 
  scale_fill_grey(start = 0, end = 0.8) + 
  scale_color_grey(start = 0, end = 0.8) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(), 
          legend.position = "none"
          )


