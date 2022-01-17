'
  Script: Visualizations for paper
  Input: All graphs previously generated
  Output: Final figures for the paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")
source("src/upload_data.R")
source("src/upload_mab.R")
source("src/preparations/frequency_df.R")


# Figure 2 ----------------------------------------------------------------
# strain
source("src/figures/fig_strain.R")
g_strain

# model
source("src/figures/fig_model.R")
g_model

# age
source("src/figures/fig_age.R")
g_age

# brain areas
source("src/figures/fig_brainareas.R")
g_freq_ba

# outcomes by brain areas
source("src/figures/fig_outcomebrainareas.R")
g_out_ba


fig_2 <- ggpubr::ggarrange(
  
  ggpubr::ggarrange(
    g_strain, g_model, g_age, 
    nrow = 1, labels = c("A", "B", "C")
  ), 
  
  ggpubr::ggarrange(
    g_freq_ba, g_out_ba,
    nrow = 1, widths = c(0.7,1), labels = c("D", "E")
  ),
  
  nrow = 2
)


ggsave("figs/unedited/fig_2.svg", width = 10, height = 10)


# Figure 3 ----------------------------------------------------------------
# limbic overview
source("src/figures/fig_limbic_overview.R")
g_limbic_overview

# anxiety prone phenotype - acute situation
source("src/figures/fig_anxiety_prone.R")
g_anxiety_prone

source("src/figures/fig_hits.R")
g_hits

fig_space <- ggplot() + geom_blank() + 
  theme_minimal()

ggpubr::ggarrange(
  
  ggpubr::ggarrange(
    g_limbic_overview,
    
    ggpubr::ggarrange(
      fig_space,
      g_anxiety_prone, 
      nrow = 2, labels = c("", "B"), heights = c(1.5, 1)
    ),
    
    ncol = 2, labels = c("A", ""), widths = c(1, 1.3)
  ), 
  
  g_hits, 
  nrow = 2, labels = c("", "C"), heights = c(2.8,1)

)

ggsave("figs/unedited/fig_3.svg", width = 12, height = 16)



# Figure 4 ----------------------------------------------------------------
# cartoon image of ELA enhancing pre-existing differences
source("src/figures/fig_cartoon_sd_preexist.R")
g_cartoon_ela

# cvr life exp
source("src/figures/fig_cvr_life_exp.R")
g_cvr_life_exp




fig_4 <- ggpubr::ggarrange(
  g_cartoon_ela, g_cvr_life_exp, heights = c(2,3),
  ncol = 2, labels = c("A", "B")
)

ggsave("figs/unedited/fig_4.svg", width = 10, height = 5)



# Figure 6 ----------------------------------------------------------------

source("src/figures/fig_g_cvr.R")
g_g_cvr

ggsave("figs/unedited/fig_6.svg", width = 6, height = 6)

