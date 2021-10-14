## files to run

### From behavior data (mab project), select that of interest for this study
## output in raw (mab.RDS)
source("src/mab_xl_to_rds.R")

### from mab data, get eff size estimates and aggregates wherever necessary
## output in temp (mab_aggr.RDS)
source("src/mab_effsize.RDS")