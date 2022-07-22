library(dplyr)
library(jagsUI)

# Detection data
b.occ.mat.ensp_ebird <- read.csv("/scratch/mcallen/blra_pred/b.occ.mat.ensp_ebird.csv") %>% as.matrix()

# Observation level covariates (ordinal date)
b.obs.cov.ord.ensp_ebird <- read.csv("/scratch/mcallen/blra_pred/b.obs.cov.ord.ensp_ebird.csv") %>% as.matrix()

# Observation level covariates (survey duration in hours)
b.obs.cov.dur.ensp_ebird <- read.csv("/scratch/mcallen/blra_pred/b.obs.cov.dur.ensp_ebird.csv") %>% as.matrix()

# Covariate values for all survey points
b.site.cov.ensp_ebird <- read.csv("/scratch/mcallen/blra_pred/b.site.cov.ensp_ebird.csv") %>%
  filter(is.na(ss.500)==F) %>%
  mutate(yearfac = as.factor(yearfac))

# Covariate values for all tidal marsh cells
b.brick.mask.pts <- read.csv("/scratch/mcallen/blra_pred/b.brick.mask.pts.csv")[,-1] %>%
  filter(is.na(ss.500)==F)

# Create design matrix for occupancy covariates
occDM <- model.matrix(~ yearfac + ss.500 + sharp_hm.500 + sharp_tb.500 + ldev.100 + imp.500, data = b.site.cov.ensp_ebird)[,-1] # Drop first col.

# Load data for JAGS
ensp_ebird.jags.data <- 
  list(
    y = b.occ.mat.ensp_ebird, 
    M = nrow(b.occ.mat.ensp_ebird), 
    J = ncol(b.occ.mat.ensp_ebird),
    ord = b.obs.cov.ord.ensp_ebird,
    dur = b.obs.cov.dur.ensp_ebird,
    occDM = occDM, 
    ss.500_pred = b.brick.mask.pts$ss.500,
    sharp_hm.500_pred = b.brick.mask.pts$sharp_hm.500,
    sharp_tb.500_pred = b.brick.mask.pts$sharp_tb.500,
    ldev.100_pred = b.brick.mask.pts$ldev.100,
    imp.500_pred = b.brick.mask.pts$imp.500
  )
    
# Inits
inits <- function(){list(z = apply(b.occ.mat.ensp_ebird, 1, max, na.rm = T), mean.psi = runif(1), mean.p = runif(1), alpha = c(-1, 1) + rnorm(2), beta = (c(rep(0, 10), -2,-1,2, 0,-2,-2)+rnorm(16)))}

params <- c("alpha0", "alpha", "beta0", "beta", "psi.marsh.cells")

# MCMC settings
ni <- 20000   ;   nt <- 10   ;   nb <- 10000   ;   nc <- 3

# run p.ord model
ensp_ebird_p.ord_psi.yr.ss.hm.tb.ld.im <-
  jagsUI::jags(
    ensp_ebird.jags.data,
    inits,
    params,
    "/scratch/mcallen/blra_pred/ensp_ebird_cell_predict_for_Amarel.txt",
    n.chains = nc,
    n.thin = nt,
    n.iter = ni,
    n.burnin = nb,
    parallel = TRUE
  )

posteriors = ensp_ebird_p.ord_psi.yr.ss.hm.tb.ld.im$sims.list$psi.marsh.cells
write.csv(posteriors, "/scratch/mcallen/blra_pred/ensp_ebird_cell_predict_out_dur.csv")