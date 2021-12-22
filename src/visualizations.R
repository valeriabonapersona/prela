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


# Figure 3 ----------------------------------------------------------------
# strain
source("src/figures/fig_strain.R")
g_strain

# model
source("src/figures/fig_model.R")
g_model
