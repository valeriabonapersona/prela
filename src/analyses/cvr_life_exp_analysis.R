'
  Script: Analysis cvr across life experience
  Input: data cvr from upload_data
  Output: results cvr analysis across life experience
  
  Author: v.bonapersona-2@umcutrecht.nl

'


mod_cvr_life_exp <- my_rma(my_dat = dat_cvr, abs = F, var = "life_exp", intercept = F)
res_cvr_life_exp <- mv_to_df(dat_cvr, "life_exp", mod_cvr_life_exp)

rm(mod_cvr_life_exp)