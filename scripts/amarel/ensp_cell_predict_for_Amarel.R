library(dplyr)
library(jagsUI)

# Detection data
b.occ.mat <- read.csv("/scratch/mcallen/blra_pred/b.occ.mat.csv")

# Observation level covariates (ordinal date)
b.obs.cov.ord2 <- read.csv("/scratch/mcallen/blra_pred/b.obs.cov.ord2.csv") %>% as.matrix()

# Covariate values for all survey points
b.site.cov <- read.csv("/scratch/mcallen/blra_pred/b.site.cov.csv") %>%
  filter(is.na(ss.500)==F)

# Covariate values for all tidal marsh cells
b.brick.mask.pts <- read.csv("/scratch/mcallen/blra_pred/b.brick.mask.pts.csv")[,-1] %>%
  filter(is.na(ss.500)==F)

# Create design matrix for occupancy covariates
occDM <- model.matrix(~ year2016 + year2018 + year2019 + ss.500 + sharp_hm.500 + sharp_tb.500 + ldev.100 + imp.500, data = b.site.cov)[,-1] # Drop first col.


# Bundle and summarize data set
ensp.jags.data <-
  list(
    y = b.occ.mat,
    M = nrow(b.occ.mat),
    J = ncol(b.occ.mat),
    ord = b.obs.cov.ord2 / 100,
    hrs = b.obs.cov.hrs_midnite2,
    ord2 = (b.obs.cov.ord2 / 100) ^ 2,
    hrs2 = b.obs.cov.hrs_midnite2 ^ 2,
    ordxhrs = (b.obs.cov.ord2 / 100) * b.obs.cov.hrs_midnite2,
    occDM = occDM,
    ss.500_pred = b.brick.mask.pts$ss.500,
    sharp_hm.500_pred = b.brick.mask.pts$sharp_hm.500,
    sharp_tb.500_pred = b.brick.mask.pts$sharp_tb.500,
    ldev.100_pred = b.brick.mask.pts$ldev.100,
    imp.500_pred = b.brick.mask.pts$imp.500
  )

# Inits (single detection covariate)
inits.one.p.cov <- function(){list(z = apply(b.occ.mat, 1, max, na.rm = T), mean.psi = runif(1), mean.p = runif(1), alpha = -1+rnorm(1), beta = (c(3,-3,-5,-3,3,5,-5, 2)+rnorm(8)))}

params_pred <- c("alpha0", "alpha", "beta0", "beta", "psi.marsh.cells")

# MCMC settings
ni <- 20000   ;   nt <- 10   ;   nb <- 10000   ;   nc <- 3

# run p.ord model
ensp_p.ord_psi.y16.y18.y19.ss.hm.tb.ld.im <-
  jagsUI::jags(
    ensp.jags.data,
    inits.one.p.cov,
    params_pred,
    "/scratch/mcallen/blra_pred/ensp_cell_predict_for_Amarel.txt",
    n.chains = nc,
    n.thin = nt,
    n.iter = ni,
    n.burnin = nb,
    parallel = TRUE
  )

posteriors = ensp_p.ord_psi.y16.y18.y19.ss.hm.tb.ld.im$sims.list$psi.marsh.cells
write.csv(posteriors, "/scratch/mcallen/blra_pred/ensp_cell_predict_out.csv")
