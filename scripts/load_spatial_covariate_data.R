# Load and format spatial covariate data

# read in New Jersey NWI data from shapefile
# reclassify using Stevens & Conway (2018) terminology
# download here: https://www.fws.gov/wetlands/data/State-Downloads.html
# nwi_defs <- read_csv("data/NWI_Code_Definitions.csv")
# nwi <-
#   read_sf("data/big_files/NJ_Wetlands.shp") %>%
#   st_transform(crs = map_proj) %>%
#   left_join(nwi_defs, by = "ATTRIBUTE") %>%
#   mutate(
#     sc_system = as.numeric(case_when(
#       substr(ATTRIBUTE, 1, 2) == "L1" ~ 1, # "Lacustrine Limnetic",
#       substr(ATTRIBUTE, 1, 2) == "L2" ~ 2, # "Lacustrine Littoral",
#       substr(ATTRIBUTE, 1, 1) == "P" ~ 3, # "Palustrine",
#       substr(ATTRIBUTE, 1, 2) == "R2" ~ 4, # "Riverine Lower-Perennial",
#       substr(ATTRIBUTE, 1, 2) == "R3" ~ 5, # "Riverine Upper-Perennial",
#       substr(ATTRIBUTE, 1, 2) == "R4" ~ 6, # "Riverine Intermittent",
#       TRUE ~ as.numeric(NA))
#       ),
#     sc_class = as.numeric(case_when(
#       CLASS_NAME == "Emergent" ~ 1, # "Emergent",
#       CLASS_NAME == "Scrub-Shrub" ~ 2, # "Scrub-Shrub",
#       CLASS_NAME == "Forested" ~ 3, # "Forested",
#       CLASS_NAME == "Rocky Shore" ~ 4, # "Shore",
#       CLASS_NAME == "Unconsolidated Shore" ~ 4, # "Shore",
#       CLASS_NAME == "Rock Bottom" ~ 5, # "Water",
#       CLASS_NAME == "Unconsolidated Bottom" ~ 5, # "Water",
#       CLASS_NAME == "Aquatic Bed" ~ 5, # "Water",
#       CLASS_NAME == "Streambed" ~ 5, # "Water",
#       TRUE ~ as.numeric(NA))
#     ),
#     sc_regime = as.numeric(case_when(
#       WATER_REGIME_NAME == "Permanently Flooded" ~ 1, # "Permanently Flooded",
#       WATER_REGIME_NAME == "Intermittently Exposed" ~ 2, # "Intermittently Exposed",
#       WATER_REGIME_NAME == "Semipermanently Flooded" ~ 3, # "Semipermanently Flooded",
#       WATER_REGIME_NAME == "Seasonally Flooded" ~ 4, # "Seasonally Flooded",
#       WATER_REGIME_NAME == "Saturated" ~ 5, # "Saturated",
#       WATER_REGIME_NAME == "Temporarily Flooded" ~ 6, # "Temporarily Flooded",
#       WATER_REGIME_NAME == "Intermittently Flooded" ~ 7, # "Intermittently Flooded",
#       WATER_REGIME_NAME == "Artificially Flooded" ~ 8, # "Artificially Flooded",
#       TRUE ~ as.numeric(NA))
#     ),
#     sc_modifier = as.numeric(case_when(
#       FIRST_MODIFIER_NAME == "Diked/Impounded" ~ 1, # "Diked/Impounded",
#       FIRST_MODIFIER_NAME == "Excavated" ~ 2, # "Excavated",
#       TRUE ~ as.numeric(NA))
#     )
#   ) %>%
#   dplyr::select(
#     ATTRIBUTE,
#     sc_system,
#     sc_class,
#     sc_regime,
#     sc_modifier,
#     WETLAND_TY,
#     SYSTEM_NAME,
#     SUBSYSTEM_NAME,
#     CLASS_NAME,
#     WATER_REGIME_NAME,
#     FIRST_MODIFIER_NAME,
#     geometry
#   )

# Rasterize NWI data
# #nwi_system = fasterize(nwi, gap, field = "sc_system")
# nwi_class = fasterize(nwi, gap, field = "sc_class")
# #nwi_regime = fasterize(nwi, gap, field = "sc_regime")
# nwi_modifier = fasterize(nwi, gap, field = "sc_modifier")

# read in SHARP marsh zonation maps
# downloaded from: https://www.tidalmarshbirds.org/?page_id=2168
#memory.limit(size = 999999)
#mz_z4 <-
#  raster("data/big_files/Zone4_DEM.tif")
#mz_z5 <-
#  raster("data/big_files/Zone5_DEM.tif")
#mz_z6 <-
#  raster("data/big_files/Zone6_DEM.tif")
#mz_z456 <- 
#  merge(mz_z4, mz_z5, mz_z6, tolerance = 0.5)
#mz_z456 <-
#  raster("data/big_files/marsh_zonation.tif") # or this pre-combined version
#m <- 
#  matrix(c(1,2,3,4,5,6,7,8,9,NA,1,rep(0,9)), ncol = 2)
#mz_hm <- 
#  reclassify(mz_z456, rcl = m) # reclassify as 1 / 0 (high marsh / not)
#mz_hm30 <- 
#  aggregate(mz_hm, fact = 10, fun = mean) # mean of 1's and 0's (%) in 30 m x 30 m
#sharp_hm <- 
#  projectRaster(mz_hm30, gap, method = "bilinear") # match up with 30 x 30 gap file
# sharp_hm <- 
#   raster("data/big_files/sharp_hm.tif") # read in to skip above steps 

# make terrestrial border raster from SHARP marsh zonation raster
#m2 <- 
#  matrix(c(1,2,3,4,5,6,7,8,9,NA,0,0,0,1,rep(0,6)), ncol = 2)
#mz_tb <- 
#  reclassify(mz_z456, rcl = m2) # reclassify as 1 / 0 (tb / not)
#mz_tb30 <- 
#  aggregate(mz_tb, fact = 10, fun = mean) # mean of 1's and 0's (%) in 30 m x 30 m
#writeRaster(mz_tb30, "data/big_files/sharp_tb_utm.tif")
#mz_tb30 <-
#  raster("data/big_files/sharp_tb_utm.tif")
#sharp_tb <- 
#  projectRaster(mz_tb30, gap, method = "bilinear") # match up with 30 x 30 gap file
# sharp_tb <-
#   raster("data/big_files/sharp_tb.tif") # read in to skip above steps 

# Read in NJ GAP land cover data
# Downloaded here: https://www.usgs.gov/core-science-systems/science-analytics-and-synthesis/gap/science/land-cover-data-download
# gap <- 
#   raster("data/big_files/gaplf2011lc_v30_nj.tif")

# make low intensity development raster from GAP land cover raster
#m.ldev <- 
#  matrix(c(1:584,NA,rep(0,581),1,0,0,0), ncol = 2)
#gap_ldev <- 
#  reclassify(gap, rcl = m.ldev) # reclassify as 1 / 0 (low-int development / not)
# gap_ldev <- 
#   raster("data/big_files/gap_ldev.tif") # read in to skip above steps 

# Formatting environmental variables
#Make rasters with pixels = % cover within 500 m buffer area. Following Stevens & Conway ACJV model.

# Load functions to calculate % cover 
# source("scripts/pland_functions.R")

# 200 m buffer of the 4ft NOAA Sea Level Rise inundation raster layer
# load to mask covariate rasters so they only have values near coast
# pred.area <-
#   st_read("data/shapefiles/NJ_All_4ft_poly_Albers_200.shp") %>%
#   st_transform(crs = map_proj)

# outline of NJ used to clip 4ft SLR layer (pred.area) so it doesn't include ocean or DE bay
# nj_outline <- st_read("data/shapefiles/nj_counties.shp") %>%
#   st_transform(crs = map_proj)

# 500 m buffer rasters for the NWI variables (takes ~10-15 min each)

# scrub-shrub
# ss.500 <- pland.nwi(nwi_class, 2, 500) %>%
#  mask(mask = pred.area) %>%
#  mask(mask = nj_outline) %>%
#  crop(pred.area)
# ss.500 = raster("data/big_files/ss.500.tif") # to save time

# artificially flooded (NWI regime code 8): all zeros in NJ; no need to model

# high marsh
# sharp_hm.500 <- pland.sharp(sharp_hm, 500) %>%
#  mask(mask = pred.area) %>%
#  mask(mask = nj_outline) %>%
#  crop(pred.area)
# sharp_hm.500 = raster("data/big_files/sharp_hm.500.tif") # to save time

# terrestrial border
# sharp_tb.500 <- pland.sharp(sharp_tb, 500) %>%
#  mask(mask = pred.area) %>%
#  mask(mask = nj_outline) %>%
#  crop(pred.area)
# sharp_tb.500 = raster("data/big_files/sharp_tb.500.tif") # to save time

# low-intensity development (100 m buffer; used in S&C ConSciPrac 2020)
# ldev.100 <- pland.sharp(gap_ldev, 100) %>%
#  mask(mask = pred.area) %>%
#  mask(mask = nj_outline) %>%
#  crop(pred.area)
# ldev.100 = raster("data/big_files/ldev.100.tif") # to save time

# impounded
# imp.500 <- pland.nwi(nwi_modifier, 1, 500) %>%
#  mask(mask = pred.area) %>%
#  mask(mask = nj_outline) %>%
# crop(pred.area)
# imp.500 = raster("data/big_files/imp.500.tif") # to save time

# remove 4 ft SLR polygon file from memory
# rm(pred.area, nj_outline)

# b.brick <- 
#   brick(ss.500, sharp_hm.500, pat.500, sharp_tb.500, ldev.100, imp.500)
# writeRaster(test, "data/big_files/b.brick2.tif")
b.brick <- brick("D:/black_rail/big_output_2021_10/tif/b.brick.tif") # to save time
names(b.brick) <-
  c("ss.500",
    "sharp_hm.500",
    "sharp_tb.500",
    "ldev.100",
    "imp.500")
# plot(b.brick)

# Raster to mask tidal marsh prediction area
# includes only 30x30 cells contained within NJ emergent tidal marsh
# pred_mask <-
#   raster("D:/black_rail/big_output_2021_10/tif/pred_mask.tif")

# # read individual environmental covariate rasters for further masking
# ss.500 <- b.brick$ss.500
# sharp_hm.500 <- b.brick$sharp_hm.500
# sharp_tb.500 <- b.brick$sharp_tb.500
# ldev.100 <- b.brick$ldev.100
# imp.500 <- b.brick$imp.500
# 
# # Mask covariate rasters to prediction area (emergent tidal marsh)
# ss.500_pred = ss.500 %>%
#   mask(pred_mask)
# sharp_hm.500_pred = sharp_hm.500 %>%
#   mask(pred_mask)
# sharp_tb.500_pred = sharp_tb.500 %>%
#   mask(pred_mask)
# ldev.100_pred = ldev.100 %>%
#   mask(pred_mask)
# imp.500_pred = imp.500 %>%
#   mask(pred_mask)

# Make raster brick of covariates, only cells within tidal marsh
# b.brick_pred <- brick(ss.500_pred,
#                       sharp_hm.500_pred,
#                       sharp_tb.500_pred,
#                       ldev.100_pred,
#                       imp.500_pred)
# writeRaster(b.brick_pred, "data/big_files/b.brick_pred2.tif")
# 
# b.brick_pred <-
#   brick("data/big_files/b.brick_pred.tif") # to save time
# names(b.brick_pred) <- c("ss.500_pred",
#                          "sharp_hm.500_pred",
#                          "sharp_tb.500_pred",
#                          "ldev.100_pred",
#                          "imp.500_pred")

# Make a data.frame of all environmental covariates in all cells in emergent tidal marsh
# b.brick_pred_pts <- rasterToPoints(b.brick_pred)
# write.csv(b.brick_pred_pts, "data/big_files/b.brick_pred_pts2.csv", row.names = F)
b.brick_pred_pts <- 
  read.csv("D:/black_rail/big_output_2021_10/csv/b.brick_pred_pts.csv") # to save time