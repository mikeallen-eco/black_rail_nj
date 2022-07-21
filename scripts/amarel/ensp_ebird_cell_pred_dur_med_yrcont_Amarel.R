
# for Amarel HPC

library(mclapply)
library(dplyr)

ee <-
  readRDS("/scratch/mcallen/blra_pred_ycont/JAGS_ensp_ebird_p.ord.dur_psi.yrcont.ss.hm.tb.ld.im_12000iter.rds")

beta <- ee$sims.list$beta
beta0 <- ee$sims.list$beta0
pp <- readRDS("/scratch/mcallen/blra_pred_ycont/b.brick_pred_pts.rds")

ensp_ebird_cell_pred_dur_med_yrcont = mclapply(1:nrow(pp), 
       FUN = function(x){
         median(plogis(sapply(1:12000, function(y){
           beta0[y] +
           (beta[y,1] * (8-4.889157)) +
             (beta[y,2] * pp[x,3]) +
             (beta[y,3] * pp[x,4]) +
             (beta[y,4] * pp[x,5]) +
             (beta[y,5] * pp[x,6]) +
             (beta[y,6] * pp[x,7])
           })))
         
       }, mc.cores = 10) %>%
  do.call(c, .) %>%
  as.data.frame() %>%
  rename(med = 1)

# for Amarel HPC
write.csv(ensp_ebird_cell_pred_dur_med_yrcont, 
 "/scratch/mcallen/blra_pred_ycont/ensp_ebird_cell_pred_dur_med_yrcont.csv",
          row.names = F)    
    
    