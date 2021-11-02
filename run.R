## files to run

### From behavior data (mab project), select that of interest for this study
## output in raw (mab.RDS)
source("src/mab_xl_to_rds.R")

### from excel sheet, organized data on publications and experiments
source("src/exp_xls_to_rds.R")

### merge all data from prela. Outputs in temp an unprocessed prela file (prela_data_unprocessed.RDS)
source("src/merge_data.R")