# Loading and formatting spatial ownership data
# open <- 
#   st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/openspace_clip_repaired.shp") %>%
#   st_transform(crs = map_proj) %>%
#   st_buffer(dist = 0) 
# aggregate(open$GISACRES, by = list(open$OWNERTYPE), sum) # to view acreage by type

# marine protected area file for Federal protected areas
# mpa <- 
#   st_read("D:/black_rail/big_data_2021_10/mpa/NOAA_MPAI_2020_IUCN_clip_repaired.shp") %>%
#   st_transform(crs = map_proj) %>%
#   st_buffer(dist = 0) 
# aggregate(mpa$AreaKm, by = list(mpa$Gov_Level), sum) # to view area in km2 by type

# Non-protected parcels
# created from state MOD-IV parcel data
# clipped to 4 ft SLR zone in ArcGIS
# protected areas (from above open and mpa layers above) subtracted in ArcGIS
# npp <-
#   sf::read_sf("D:/black_rail/big_data_2021_10/landowner_shapefiles/nonprotected_parcels.shp") %>%
#   st_transform(crs = map_proj) %>%
#   mutate(group = "A") %>%
#   group_by(group) %>%
#   summarise() %>%
#   ungroup()
# note: this was then clipped to NWI marsh extent
#       and edited in ArcGIS to remove more fed/state/nonprofit lands
#       as discovered via queries of OWNER_NAME in ArcGIS
#       this resulted in the file npp_parcels_owners_marsh.shp.
#       The queries used to find the fed/state/nonprofit parcels are listed below.
#       More public/nonprofit lands are then removed below using queries in R
#       The queries for the initial removal of private parcels are:
# "UNITED STATES"
# "STATE OF NEW JERSEY"
# "NEW JERSEY NATURAL LANDS"
# "NEW JERSEY DEP"
# "NEW JERSEY ST"
# "NEW JERSEY STATE"
# "NEW JERSEY DOT"
# "NEW JERSEY TURNPIKE"
# "NEW JERSEY MEADOWLANDS"
# "NEW JERSEY AUDUBON" 
# "NATURE CONSERVANCY"
# "NEW JERSEY CONSERVATION"

# note that some of the above queries may have been overly broad
# so additional filters may have been needed to prevent removal of private lands

# load npp_parcels_owners_marsh.shp 
npp_owners_marsh_raw <-
  st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/npp_parcels_owners_marsh.shp") %>% 
  st_transform(crs = map_proj)

# load all full parcels that intersect NWI emergent tidal marsh (not clipped)
# this is for parcel area calculations
parcels_nwi_full <- st_read("D:/black_rail/big_data_2021_10/modiv/parcel_nwi_owners.shp") %>%
  st_transform(crs = map_proj) %>%
  mutate(par_ha = as.numeric(st_area(.)/10000)) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  mutate(calc_ha = CALC_ACRE * 0.404686) %>%
  dplyr::select(PAMS_PIN, calc_ha, par_ha)

# pull out county lands from privately-owned parcels
other_county = npp_owners_marsh_raw %>%
  filter(grepl("COUNTY", OWNER_NAME) |
           grepl("CAPE MAY CO VOCATIONAL SCHOOL", OWNER_NAME) |
           grepl("ATLANTIC COUNTY SEWERAGE AUTHORITY", OWNER_NAME) |
           grepl("BERGEN COUNTY SEWER AUTH", OWNER_NAME) |
           grepl("MIDDLESEX COUNTY SEWERAGE", OWNER_NAME) |
           grepl("OCEAN COUNTY SEWERAGE AUTHORITY", OWNER_NAME) |
           grepl("TWO RIVERS WATER RECLAMATION AUTH.", OWNER_NAME) # Monmouth County
  ) %>%
  # next line is to keep true private parcels from being removed
  filter(!grepl("LLC", OWNER_NAME)) %>%
  arrange(OWNER_NAME)

#  pull out mun lands from privately-owned parcels
other_mun = npp_owners_marsh_raw %>%
  filter(grepl("TOWN", OWNER_NAME) |
           grepl("TWP", OWNER_NAME) |
           grepl("CITY", OWNER_NAME) |
           grepl("BOARD OF EDUCATION", OWNER_NAME) |
           grepl("BD OF ED", OWNER_NAME) |
           grepl("BORO", OWNER_NAME) |
           grepl("BOROOUGH", OWNER_NAME) |
           grepl("BOROUGH", OWNER_NAME) |
           grepl("LOWER CAPE MAY REGIONAL SCHOOL", OWNER_NAME) |
           grepl("LINDEN ROSELLE SEWERAGE AUTHORITY", OWNER_NAME) |
           grepl("RAHWAY VALLEY SEWERAGE AUTHORITY", OWNER_NAME) | # mainly Woodbridge
           grepl("BAYSHORE REGIONAL SEWERAGE AUTH", OWNER_NAME) | # Union Beach & vicinity
           grepl("TWP OF MIDDLETOWN - SEWERAGE AUTH", OWNER_NAME) 
  ) %>%
  filter(!grepl("TOWNSEND", OWNER_NAME) &
           !grepl("TOWNSLEY", OWNER_NAME) &
           !grepl("TOWNHOUSE", OWNER_NAME) &
           !grepl("CHAPEL", OWNER_NAME) &
           !grepl("TOWNE", OWNER_NAME) &
           !grepl("LLC", OWNER_NAME) &
           !grepl("ATLANTIC CITY ELEC", OWNER_NAME) &
           !grepl("HILLSBORO", OWNER_NAME) &
           !grepl("EDGEBORO, INC", OWNER_NAME) &
           !grepl("EDGEBORO INC", OWNER_NAME)) %>%
  arrange(OWNER_NAME)

#  pull out federal lands from privately-owned parcels
other_fed = npp_owners_marsh_raw %>%
  filter(grepl("NJ NATL GUARD ARMORY DEPT OF DEFENS", OWNER_NAME) |
           grepl("US GOV", OWNER_NAME) |
           grepl("US DEPT OF THE INTERIOR", OWNER_NAME) |
           grepl("US FISH AND WILDLIFE SERVICE", OWNER_NAME) |
           grepl("USA ARMY ENGINEERS - WANAMAKER BLDG", OWNER_NAME) |
           grepl("U.S.A.", OWNER_NAME) |
           grepl("U S POSTAL SERVICE", OWNER_NAME) |
           grepl("U S OF AMERICA", OWNER_NAME) |
           grepl("USA % FORSYTHE REFUGE", OWNER_NAME) |
           grepl("USA FISH & WILDLIFE SERVICE", OWNER_NAME) |
           grepl("USCG LORAN SUPPORT UNIT", OWNER_NAME) |
           grepl("USCG FINANCE CENTER WATER/SEWER", OWNER_NAME)
  ) %>%
  filter(!grepl("EQUISTAR CHEMICALS", OWNER_NAME) &
           !grepl("CUESTA", OWNER_NAME) &
           !grepl("CHISMAR", OWNER_NAME) &
           !grepl("CHEVRON U S A  INC", OWNER_NAME))

#  pull out nonprofit lands from privately-owned parcels
other_nonp = npp_owners_marsh_raw %>%
  filter(grepl("THE NJ CONSERVATION FOUNDATION", OWNER_NAME) |
           grepl("N J  CONSERVATION FOUNDATION", OWNER_NAME) |
           grepl("NATURAL LAND TRUST INC", OWNER_NAME) |
           grepl("BARNEGAT BAY CONSERVANCY LLC", OWNER_NAME) |
           grepl("SOUTH JERSEY WETLANDS INSTITUTE", OWNER_NAME)
  )

#  pull out state lands from privately-owned parcels
other_state = npp_owners_marsh_raw %>%
  filter(grepl("D E P", OWNER_NAME) |
           grepl("NJ MEADOWLANDS COMM.", OWNER_NAME) |
           grepl("NJ MEADOWLNDS COMM", OWNER_NAME) |
           grepl("THE NJ MEADOWLANDS COMM", OWNER_NAME) |
           grepl("NJ SPORTS EXPOSITION AUTHORITY", OWNER_NAME) |
           grepl("NJ SPORTS & EXPO AUTHORITY", OWNER_NAME) |
           grepl("NJ FISH & GAME COMMISSION", OWNER_NAME) |
           grepl("NJ HIGHWAY AUTHORITY", OWNER_NAME) |
           grepl("NJ TURNPIKE AUTHORITY", OWNER_NAME) |
           grepl("NJ TPKE AUTHORITY", OWNER_NAME) |
           grepl("NJ TRANSIT CORP", OWNER_NAME) |
           grepl("NEW JERSEY TRANSIT CORP", OWNER_NAME) |
           grepl("NEW JERSEY TRANSIT CORPORATION", OWNER_NAME) |
           grepl("NJ TRANSIT % RAIL OPERATIONS", OWNER_NAME) |
           grepl("NJ TRANSIT", OWNER_NAME) |
           grepl("NJ STATE POLICE", OWNER_NAME) |
           grepl("NJ STATE OF ENV PROTECTION", OWNER_NAME) |
           grepl("NJ STATE", OWNER_NAME) |
           grepl("ROWAN UNIVERSITY OF NEW JERSEY", OWNER_NAME) |
           grepl("NEW JERSEY HIGHWAY AUTHORITY", OWNER_NAME) |
           grepl("NEW JERSEY EXPRESSWAY AUTHORITY", OWNER_NAME) |
           grepl("NEW JERSEY EXPRESSWAY AUTH", OWNER_NAME) |
           grepl("RUTGERS THE STATE UNIVERSITY", OWNER_NAME) |
           grepl("RUTGERS UNIV/REAL EST ADMIN-DIRECTR", OWNER_NAME) |
           grepl("N.J.TURNPIKE AUTH.", OWNER_NAME) |
           grepl("N J EXPRESSWAY AUTHORITY", OWNER_NAME) |
           grepl("N J HIGHWAY AUTHORITY", OWNER_NAME) |
           grepl("TURNPIKE AUTHORITY % J BRIXIE", OWNER_NAME) |
           grepl("PORT AUTHORITY", OWNER_NAME) |
           grepl("PORT AUTHORITY OF NY & NJ", OWNER_NAME) |
           grepl("PORT OF NY AUTHORITY", OWNER_NAME) |
           grepl("HACKENSACK MEADOWLANDS DEV.COM", OWNER_NAME) |
           grepl("NJMC", OWNER_NAME) |
           grepl("NJSEA", OWNER_NAME) |
           grepl("MEADOWLANDS CONSERV TRUST", OWNER_NAME) |
           grepl("SOUTH JERSEY TRANSPORTATION AUTH", OWNER_NAME) |
           grepl("SOUTH JERSEY PORT CORPORATION", OWNER_NAME)
  ) %>%
  st_make_valid() 

# Remove these public/nonprofit parcels from npp_owners_marsh
npp_owners_marsh_cleaned <- npp_owners_marsh_raw %>%
  filter(OWNER_NAME %notin% other_county$OWNER_NAME) %>%
  filter(OWNER_NAME %notin% other_mun$OWNER_NAME) %>%
  filter(OWNER_NAME %notin% other_fed$OWNER_NAME) %>%
  filter(OWNER_NAME %notin% other_nonp$OWNER_NAME) %>%
  filter(OWNER_NAME %notin% other_state$OWNER_NAME) %>%
  arrange(OWNER_NAME)

# check and fix 11 invalid polygons; shapes and areas of all polygons remained the same
# check_invalid_parcels = npp_owners_marsh2[st_is_valid(npp_owners_marsh2)==F,]
npp_owners_marsh <- st_make_valid(npp_owners_marsh_cleaned) %>%
  # add in areas of full (not clipped) parcels
  left_join(parcels_nwi_full, by = "PAMS_PIN") %>%
  mutate(marsh_ha = as.numeric(st_area(.) / 10000),
         owner_known = ifelse(is.na(OWNER_NAME)==T, "N", "Y"))

# Manually scan for public/nonprofit owners that should not be included
# check = filter(npp_owners_marsh, substr(OWNER_NAME,1,1)=="Z") %>%
#   arrange(OWNER_NAME) %>% dplyr::select(PAMS_PIN, OWNER_NAME, COUNTY, CALC_ACRE)
# checked all manually by going through each letter of the alphabet

# dissolve the privately-owned parcels into a single polygon
# npp_owners_marsh_dis <- npp_owners_marsh %>%
#   group_by() %>%
#   summarise()

# npp_owners_marsh_for_shp <- npp_owners_marsh %>%
#   mutate(netval_1k = NET_VALUE/1000) %>%
#   dplyr::select(PAMS_PIN, OWNER_NAME, netval_1k, ZIP5, par_ha, marsh_ha)
  
# final version to shp (gave error message about NET_VALUE number length)
# st_write(npp_owners_marsh_for_shp, 
#           "D:/black_rail/big_data_2021_10/landowner_shapefiles/npp_owners_marsh3.shp")
# note: npp_owners_marsh3.shp created 3/25/2021 after full manual check of all private parcels
 
# subset and dissolve protected areas by ownership
# State protected areas
# state_PA <- open %>%
#   filter(OWNERTYPE == "State") %>%
#   mutate(type = 1) %>%
#   group_by(type) %>%
#   summarise() %>%
#   ungroup()
# st_write(state_PA, "D:/black_rail/big_data_2021_10/landowner_shapefiles/state_PA.shp")
# note: this was then edited in ArcGIS to add in state parcels not listed in open space file
# including NJ DEP, Meadowlands Commission ('quasi-state'), and other 'authorities'
# list of criteria to find parcels is in the file "GIS_ownership_methods.txt"
# and clipped to NWI emergent tidal marsh to make this version (state_PA_marsh6.shp)...
# state_PA_marsh <-
#   st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/state_PA_marsh6.shp") %>%
#   st_make_valid() # doesn't change area at all

# Add more polygons owned by the state, but not in the PA shapefile here
#   these are the polygons created above called "other_state"

# other_state_dis <- other_state %>%
#   group_by() %>%
#   summarise()
# state_PA_marsh = state_PA_marsh %>%
#   st_union(st_make_valid(other_state_dis), by_feature = F, is_coverage = T)
# st_write(state_PA_marsh, "D:/black_rail/big_data_2021_10/landowner_shapefiles/state_PA_marsh8.shp")
# note: state_PA_marsh8.shp created 3/25/2021 after full manual check of all private parcels

# state_PA_marsh <-
#   st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/state_PA_marsh8.shp")

# Municipal protected areas
# mun_PA <- open %>%
#   filter(OWNERTYPE == "Municipal") %>%
#   mutate(type = 1) %>%
#   group_by(type) %>%
#   summarise() %>%
#   ungroup()
# st_write(mun_PA, "D:/black_rail/big_data_2021_10/landowner_shapefiles/mun_PA.shp")

#clipped in ArcGIS to NWI emergent tidal marsh
# mun_PA_marsh <-
#   st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/mun_PA_marsh.shp")

# Added in municipal/city lands that were not considered PAs (other_mun above)
# other_mun_dis <- other_mun %>%
#   group_by() %>%
#   summarise()
# mun_PA_marsh = mun_PA_marsh %>%
#     st_union(st_make_valid(other_mun_dis), by_feature = F, is_coverage = T)
# st_write(mun_PA_marsh, "D:/black_rail/big_data_2021_10/landowner_shapefiles/mun_PA_marsh3.shp")
# note: mun_PA_marsh3.shp created 3/25/2021 after full manual check of all private parcels

# final version of municipal-owned marsh
# mun_PA_marsh <-
#   st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/mun_PA_marsh3.shp")

# County protected areas
# county_PA <- open %>%
#   filter(OWNERTYPE == "County") %>%
#   mutate(type = 1) %>%
#   group_by(type) %>%
#   summarise() %>%
#   ungroup()
# st_write(county_PA, "D:/black_rail/big_data_2021_10/landowner_shapefiles/county_PA.shp")

# clipped to NWI emergent tidal marsh
# county_PA_marsh <-
#   st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/county_PA_marsh.shp")

# Added in county lands that were not considered PAs (other_county above)
 # other_county_dis <- other_county %>%
 #   group_by() %>%
 #   summarise()
 
# county_PA_marsh = county_PA_marsh %>%
#     st_union(st_make_valid(other_county_dis), by_feature = F, is_coverage = T)
# st_write(county_PA_marsh, "D:/black_rail/big_data_2021_10/landowner_shapefiles/county_PA_marsh3.shp")
# note: county_PA_marsh3.shp created 3/25/2021 after full manual check of all private parcels

# final version of county-owned marsh
# county_PA_marsh <-
#   st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/county_PA_marsh3.shp")

# Non-profit protected areas
# nonp_PA <- open %>%
#   filter(OWNERTYPE == "Nonprofit") %>%
#   mutate(type = 1) %>%
#   group_by(type) %>%
#   summarise() %>%
#   ungroup()
# st_write(nonp_PA, "D:/black_rail/big_data_2021_10/landowner_shapefiles/nonp_PA.shp")
# note: this was then edited in ArcGIS to add in non-profit parcels not in open space file
# including NJA, NJCF, and NJ TNC
# and clipped to NWI emergent tidal marsh here..
 # nonp_PA_marsh <-
 #  st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/nonp_PA_marsh3.shp") %>%
 #   st_make_valid()
  
# Added in nonprofit lands that were not considered PAs (other_nonp above)
  # other_nonp_dis <- other_nonp %>%
  #   group_by() %>%
  #   summarise()

# nonp_PA_marsh = nonp_PA_marsh %>%
#      st_union(st_make_valid(other_nonp_dis), by_feature = F, is_coverage = T)
# st_write(nonp_PA_marsh, "D:/black_rail/big_data_2021_10/landowner_shapefiles/nonp_PA_marsh5.shp")
# note: nonp_PA_marsh5.shp created 3/25/2021 after full manual check of all private parcels

# final version of nonprofit-owned marsh
# nonp_PA_marsh <-
#   st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/nonp_PA_marsh5.shp")

# Federal protected areas
# fed_PA <- mpa %>%
#   filter(Gov_Level == "Federal") %>%
#   mutate(type = 1) %>%
#   group_by(type) %>%
#   summarise() %>%
#   ungroup()
# st_write(fed_PA, "D:/black_rail/big_data_2021_10/landowner_shapefiles/fed_PA.shp")
# note: this was then edited in ArcGIS to add in federal parcels not listed in open space file
# including USFWS & JCNEER NOAA/Rutgers managed parcels
# and clipped to NWI emergent tidal marsh here..
 # fed_PA_marsh <-
 #  st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/fed_PA_marsh3.shp") %>%
 #   st_make_valid()
 
# Added in federal lands that were not considered PAs (other_fed above)
  # other_fed_dis <- other_fed %>%
  #   group_by() %>%
  #   summarise()

# fed_PA_marsh = fed_PA_marsh %>%
#      st_union(st_make_valid(other_fed_dis), by_feature = F, is_coverage = T)
# st_write(fed_PA_marsh, "D:/black_rail/big_data_2021_10/landowner_shapefiles/fed_PA_marsh5.shp")
# note: fed_PA_marsh5.shp created 3/25/2021 after full manual check of all private parcels
  
# final version of federally-owned marsh
# fed_PA_marsh <-
#   st_read("D:/black_rail/big_data_2021_10/landowner_shapefiles/fed_PA_marsh5.shp")

# remove unneeded objects
rm(other_mun, other_county, other_state, other_nonp, other_fed, parcels_nwi_full,
   npp_owners_marsh_raw, npp_owners_marsh_cleaned)
