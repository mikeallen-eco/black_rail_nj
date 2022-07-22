library(dplyr)
library(data.table)

ensp_ebird_cell_predict_out_summary1 = fread("/scratch/mcallen/blra_pred/ensp_ebird_cell_predict_out_dur.csv",
                                       data.table = FALSE,
                                       select = c(2:430150)) %>%
  as.matrix() %>%
  apply(2L, function(x)quantile(x, c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))) %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame() %>%
  rename(p2.5 = 1, p5 = 2, p10 = 3, med = 4, p90 = 5, p95 = 6, p97.5 = 7)

ensp_ebird_cell_predict_out_summary2 = fread("/scratch/mcallen/blra_pred/ensp_ebird_cell_predict_out_dur.csv",
                                       data.table = FALSE,
                                       select = c(430151:860299)) %>%
  as.matrix() %>%
  apply(2L, function(x)quantile(x, c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975))) %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame() %>%
  rename(p2.5 = 1, p5 = 2, p10 = 3, med = 4, p90 = 5, p95 = 6, p97.5 = 7)

ensp_ebird_cell_predict_out_summary <-
  ensp_ebird_cell_predict_out_summary1 %>%
  bind_rows(ensp_ebird_cell_predict_out_summary2)

write.csv(ensp_ebird_cell_predict_out_summary, 
          "/scratch/mcallen/blra_pred/ensp_ebird_cell_predict_out_summary_dur.csv")