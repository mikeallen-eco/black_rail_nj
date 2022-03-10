# This code loads the latest ownership polygon files as created in
    # "load_spatial_ownership_data.R"
    # the reason to load them is to create the "owner_covs_data" csv file 
    # or to summarize ownership of rail habitat quality zones (quantiles)

source("scripts/load_spatial_ownership_data.R")

# PRIVATE
# dissolve the privately-owned parcels into a single polygon
npp_owners_marsh_dis <- npp_owners_marsh %>%
  group_by() %>%
  summarise()
# note: npp_owners_marsh is an sf created in "load_spatial_ownership_data.R"

# STATE
state_PA_marsh <-
  st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/state_PA_marsh8.shp") %>%
  st_transform(crs = map_proj)
# note: state_PA_marsh8.shp created 3/25/2021 after full manual check of all private parcels

# MUNICIPAL
mun_PA_marsh <-
  st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/mun_PA_marsh3.shp") %>%
  st_transform(crs = map_proj)
# note: mun_PA_marsh3.shp created 3/25/2021 after full manual check of all private parcels

# COUNTY
county_PA_marsh <-
  st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/county_PA_marsh3.shp") %>%
  st_transform(crs = map_proj)
# note: county_PA_marsh3.shp created 3/25/2021 after full manual check of all private parcels

# NONPROFIT
nonp_PA_marsh <-
  st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/nonp_PA_marsh5.shp") %>%
  st_transform(crs = map_proj)
# note: nonp_PA_marsh5.shp created 3/25/2021 after full manual check of all private parcels

# FEDERAL
fed_PA_marsh <-
  st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/fed_PA_marsh5.shp") %>%
  st_transform(crs = map_proj)
# note: fed_PA_marsh5.shp created 3/25/2021 after full manual check of all private parcels


