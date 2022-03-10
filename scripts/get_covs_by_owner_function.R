# Getting covariates of cells within emergent tidal marsh by ownership type
get_covs_by_owner <- 
  function(out_file = "D:/black_rail/owner_covs_data.csv"){
    
# load function to extract the covariate values
source("scripts/getval_PA_function.R")

# load spatial ownership data
source("scripts/load_latest_owner_polys.R")

# load spatial covariate rasters, masked to emergent tidal marsh
b.brick_pred <-
  brick("D:/black_rail/big_output_2021_10/tif/b.brick_pred.tif") # to save time
names(b.brick_pred) <- c("ss.500_pred",
                         "sharp_hm.500_pred",
                         "sharp_tb.500_pred",
                         "ldev.100_pred",
                         "imp.500_pred")

# name individual spatial covariate rasters (masked to emergent tidal marsh)
ss.500_pred <- b.brick_pred$ss.500_pred
sharp_hm.500_pred <- b.brick_pred$sharp_hm.500_pred
sharp_tb.500_pred <- b.brick_pred$sharp_tb.500_pred
ldev.100_pred <- b.brick_pred$ldev.100_pred
imp.500_pred <- b.brick_pred$imp.500_pred

# note: the next lines can take several minutes each to run

# extract covariate values from federally-owned pixels
fed_ss.500 <- ss.500_pred %>% getval_PA(fed_PA_marsh, "Federal", "ss.500")
fed_sharp_hm.500 <- sharp_hm.500_pred %>% getval_PA(fed_PA_marsh, "Federal", "sharp_hm.500")
fed_sharp_tb.500 <- sharp_tb.500_pred %>% getval_PA(fed_PA_marsh, "Federal", "sharp_tb.500")
fed_ldev.100 <- ldev.100_pred %>% getval_PA(fed_PA_marsh, "Federal", "ldev.100")
fed_imp.500 <- imp.500_pred %>% getval_PA(fed_PA_marsh, "Federal", "imp.500")

# extract covariate values from state-owned pixels
state_ss.500 <- ss.500_pred %>% getval_PA(state_PA_marsh, "State", "ss.500")
state_sharp_hm.500 <- sharp_hm.500_pred %>% getval_PA(state_PA_marsh, "State", "sharp_hm.500")
state_sharp_tb.500 <- sharp_tb.500_pred %>% getval_PA(state_PA_marsh, "State", "sharp_tb.500")
state_ldev.100 <- ldev.100_pred %>% getval_PA(state_PA_marsh, "State", "ldev.100")
state_imp.500 <- imp.500_pred %>% getval_PA(state_PA_marsh, "State", "imp.500")

# extract covariate values from privately-owned pixels
npp_ss.500 <- ss.500_pred %>% getval_PA(npp_owners_marsh_dis, "Unprotected", "ss.500")
npp_sharp_hm.500 <- sharp_hm.500_pred %>% getval_PA(npp_owners_marsh_dis, "Unprotected", "sharp_hm.500")
npp_sharp_tb.500 <- sharp_tb.500_pred %>% getval_PA(npp_owners_marsh_dis, "Unprotected", "sharp_tb.500")
npp_ldev.100 <- ldev.100_pred %>% getval_PA(npp_owners_marsh_dis, "Unprotected", "ldev.100")
npp_imp.500 <- imp.500_pred %>% getval_PA(npp_owners_marsh_dis, "Unprotected", "imp.500")

# extract covariate values from municipal- or city-owned pixels
mun_ss.500 <- ss.500_pred %>% getval_PA(mun_PA_marsh, "Municipal", "ss.500")
mun_sharp_hm.500 <- sharp_hm.500_pred %>% getval_PA(mun_PA_marsh, "Municipal", "sharp_hm.500")
mun_sharp_tb.500 <- sharp_tb.500_pred %>% getval_PA(mun_PA_marsh, "Municipal", "sharp_tb.500")
mun_ldev.100 <- ldev.100_pred %>% getval_PA(mun_PA_marsh, "Municipal", "ldev.100")
mun_imp.500 <- imp.500_pred %>% getval_PA(mun_PA_marsh, "Municipal", "imp.500")

# extract covariate values from county-owned pixels
cou_ss.500 <- ss.500_pred %>% getval_PA(county_PA_marsh, "County", "ss.500")
cou_sharp_hm.500 <- sharp_hm.500_pred %>% getval_PA(county_PA_marsh, "County", "sharp_hm.500")
cou_sharp_tb.500 <- sharp_tb.500_pred %>% getval_PA(county_PA_marsh, "County", "sharp_tb.500")
cou_ldev.100 <- ldev.100_pred %>% getval_PA(county_PA_marsh, "County", "ldev.100")
cou_imp.500 <- imp.500_pred %>% getval_PA(county_PA_marsh, "County", "imp.500")

# extract covariate values from nonprofit-owned pixels
nonp_ss.500 <-
  ss.500_pred %>% getval_PA(nonp_PA_marsh, "Nonprofit", "ss.500")
nonp_sharp_hm.500 <-
  sharp_hm.500_pred %>% getval_PA(nonp_PA_marsh, "Nonprofit", "sharp_hm.500")
nonp_sharp_tb.500 <-
  sharp_tb.500_pred %>% getval_PA(nonp_PA_marsh, "Nonprofit", "sharp_tb.500")
nonp_ldev.100 <-
  ldev.100_pred %>% getval_PA(nonp_PA_marsh, "Nonprofit", "ldev.100")
nonp_imp.500 <-
  imp.500_pred %>% getval_PA(nonp_PA_marsh, "Nonprofit", "imp.500")

# make into one data frame for later use in JAGS (for predictions)
owner_covs_data <- fed_ss.500 %>%
bind_rows(fed_sharp_hm.500,
fed_sharp_tb.500,
fed_ldev.100,
fed_imp.500,
state_ss.500,
state_sharp_hm.500,
state_sharp_tb.500,
state_ldev.100,
state_imp.500,
npp_ss.500,
npp_sharp_hm.500,
npp_sharp_tb.500,
npp_ldev.100,
npp_imp.500,
mun_ss.500,
mun_sharp_hm.500,
mun_sharp_tb.500,
mun_ldev.100,
mun_imp.500,
cou_ss.500,
cou_sharp_hm.500,
cou_sharp_tb.500,
cou_ldev.100,
cou_imp.500,
nonp_ss.500,
nonp_sharp_hm.500,
nonp_sharp_tb.500,
nonp_ldev.100,
nonp_imp.500)

write.csv(owner_covs_data, file = out_file, row.names = FALSE)
  }

# delete the code below if the function works

# rm(
#   npp_owners_marsh_dis,
#   state_PA_marsh,
#   mun_PA_marsh,
#   county_PA_marsh,
#   nonp_PA_marsh,
#   fed_PA_marsh,
#   b.brick_pred,
#   ss.500_pred,
#   sharp_hm.500_pred,
#   sharp_tb.500_pred,
#   ldev.100_pred,
#   imp.500_pred,
#   fed_ss.500,
#   fed_sharp_hm.500,
#   fed_sharp_tb.500,
#   fed_ldev.100,
#   fed_imp.500,
#   state_ss.500,
#   state_sharp_hm.500,
#   state_sharp_tb.500,
#   state_ldev.100,
#   state_imp.500,
#   npp_ss.500,
#   npp_sharp_hm.500,
#   npp_sharp_tb.500,
#   npp_ldev.100,
#   npp_imp.500,
#   mun_ss.500,
#   mun_sharp_hm.500,
#   mun_sharp_tb.500,
#   mun_ldev.100,
#   mun_imp.500,
#   cou_ss.500,
#   cou_sharp_hm.500,
#   cou_sharp_tb.500,
#   cou_ldev.100,
#   cou_imp.500,
#   nonp_ss.500,
#   nonp_sharp_hm.500,
#   nonp_sharp_tb.500,
#   nonp_ldev.100,
#   nonp_imp.500,
#   owner_covs_data
# )