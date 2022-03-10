# read in NJ Black Rail survey data from NJ DEP
# format and correct errors in the data
b = read_csv("data/blra_data.csv") %>%
  # note: this fixes a point with same name but different coordinates (pending check)
  mutate(
    lat = case_when(
      pointid == "MUL-W-205" & date == "6/18/2018" ~ as.numeric(39.54925),
      TRUE ~ as.numeric(lat)
    ),
    lon = case_when(
      pointid == "MUL-W-205" & date == "6/18/2018" ~ as.numeric(-74.48416),
      TRUE ~ as.numeric(lon)
    )
  ) %>%
  #  filter(lat < 40.1) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = map_proj) %>%
  mutate(
    date = lubridate::mdy(date),
    year = lubridate::year(date),
    time24h = format(strptime(time_start, "%I:%M:%S %p"), "%H:%M:%S"),
    dtime = round(lubridate::hour(hms(time24h)) + minute(hms(time24h)) / 60,3),
    hrs_midnite = case_when(dtime >= 12 ~ dtime - 24, 
                            TRUE ~ dtime),
    ord = lubridate::yday(date),
    pointid.year = paste0(pointid, "-", year)
  ) %>%
  arrange(pointid, date) %>%
  group_by(pointid, year) %>%
  mutate(survey = 1:length(pointid)) %>%
  ungroup() %>%
  dplyr::select(
    pointid,
    pointid.year,
    survey,
    date,
    ord,
    year,
    time_start,
    dtime,
    hrs_midnite,
    duration_broadcasts,
    duration_survey,
    pres
  ) %>%
  # note: this next part fixes 20 points with wrong time (AM/PM switched)
  mutate(
    hrs_midnite = case_when(
      dtime > 8 & dtime < 20 & hrs_midnite > 0 ~ hrs_midnite - 12,
      dtime > 8 &
        dtime < 20 & hrs_midnite < 0 ~ hrs_midnite + 12,
      TRUE ~ hrs_midnite
    )
  ) %>%
  # note: this fixes a point name in data with different name but same coordinates
  mutate(
    survey = case_when(
      pointid.year == "MUL-W-204b-2018" ~ as.numeric(2),
      pointid.year == "MUL-W-204-2018" &
        date == "2018-06-29" ~ as.numeric(3),
      TRUE ~ as.numeric(survey)
    ),
    pointid = case_when(pointid == "MUL-W-204b" ~ "MUL-W-204",
                        TRUE ~ pointid),
    pointid.year = case_when(pointid.year == "MUL-W-204b-2018" ~ "MUL-W-204-2018",
                             TRUE ~ pointid.year)
  ) %>%
  arrange(pointid.year, survey)

# load 200 m buffer of the 4ft NOAA Sea Level Rise inundation raster layer
pred.area <-
  st_read("data/shapefiles/NJ_All_4ft_poly_Albers_200.shp") %>%
  st_transform(crs = map_proj)

# clip BLRA survey points by pred.area (4 ft sea level rise + 200 m)
# note: all points fall within NJ_outline boundaries (no need to clip)
b.clip <-
  b %>%
  st_join(pred.area) %>%
  filter(is.na(Id)==F) %>%
  dplyr::select(-Id) 

# add in GIS covariate data
b2 <-
  cbind.data.frame(b.clip, as.data.frame(extract(b.brick, y = b.clip)))

# expand data.frame to include all sites, years, and visit numbers (1-6)
b.occ.long <-
  expand.grid(
    pointid = unique(b2$pointid),
    year = c(2015, 2016, 2018, 2019),
    survey = c(1:6)
  ) %>%
  mutate(pointid.year = paste0(pointid, "-", year)) %>%
  arrange(pointid.year, survey) %>%
  left_join(b2, by = c("pointid", "year", "pointid.year", "survey")) 

b.occ.wide <-
  b.occ.long %>%
  pivot_wider(
    id_cols = c(1, 2, 3, 4),
    names_from = survey,
    values_from = pres
  ) %>%
  left_join(
    distinct(
      filter(b.occ.long, is.na(sharp_hm.500) == F),
      pointid,
      year,
      ss.500,
      sharp_hm.500,
      sharp_tb.500,
      ldev.100,
      imp.500
    ),
    by = c("pointid", "year")
  ) %>%
  mutate(
    year = paste0("y", year),
    year2016 = case_when(year == "y2016" ~ 1,
                         TRUE ~ 0),
    year2018 = case_when(year == "y2018" ~ 1,
                         TRUE ~ 0),
    year2019 = case_when(year == "y2019" ~ 1,
                         TRUE ~ 0)
  )  %>%
  filter(is.na(ss.500)==F)

# detection data for static occupancy ("stacked" site-year data w/ year covariate)
b.occ.mat <-
  b.occ.wide %>%
  dplyr::select(-pointid.year,
                -pointid,
                -year,
                -ss.500,
                -sharp_hm.500,
                -sharp_tb.500,
                -ldev.100,
                -imp.500,
                -year2016,
                -year2018,
                -year2019) %>%
  as.matrix()

# site covariates for static occupancy ("stacked" site-year data w/ year covariate)
b.site.cov <-
  b.occ.wide %>%
  filter(is.na(ss.500) == F) %>%
  dplyr::select(year2016,
                year2018,
                year2019,
                ss.500,
                sharp_hm.500,
                sharp_tb.500,
                ldev.100,
                imp.500)

# observation-level covariate for static occupancy: ordinal date
b.obs.cov.ord <-
  b.occ.long %>%
  filter(is.na(ss.500) == F) %>%
  pivot_wider(id_cols = c(1, 2, 3),
              names_from = survey,
              values_from = ord) %>%
  dplyr::select(-pointid, -year) %>%
  as.matrix()

# a version with no NAs (all NAs match up with survey NAs anyway)
b.obs.cov.ord2 = b.obs.cov.ord
b.obs.cov.ord2[is.na(b.obs.cov.ord2)] <- 160 # set NA to mean date so JAGS can run

# observation-level covariate for static occupancy: hrs relative to midnight
b.obs.cov.hrs_midnite <-
  b.occ.long %>%
  filter(is.na(ss.500) == F) %>%
  pivot_wider(id_cols = c(1, 2, 3),
              names_from = survey,
              values_from = hrs_midnite) %>%
  dplyr::select(-pointid, -year) %>%
  as.matrix()
b.obs.cov.hrs_midnite2 = b.obs.cov.hrs_midnite
b.obs.cov.hrs_midnite2[is.na(b.obs.cov.hrs_midnite2)] <- -0.05601108 # set NA to mean so JAGS can run

rm(b.clip, b2)

# Load and format eBird data
BLRA_db <- 
  DBI::dbConnect(RSQLite::SQLite(), "D:/black_rail/big_data_2021_10/db/blkrai-ERD2019-ORIN-20201016-27deaba5_data.db")

# Make a dataframe out of the SQLite database
memory.limit(size = 999999)
be <- tbl(BLRA_db, "erd") %>% 
  collect() %>%
  mutate(siteid = paste0(LATITUDE, LONGITUDE))

DBI::dbDisconnect(BLRA_db) # warning message said to call this function

# dissolved outline of NJ for clipping
nj_outline_dis <- st_read("data/shapefiles/nj_counties.shp") %>%
  st_transform(crs = map_proj) %>%
  mutate(field = 1) %>%
  group_by(field) %>%
  summarize()

# put occupancy data into long format
be.occ.long <-
  sf::st_as_sf(be,
               coords = c("LONGITUDE", "LATITUDE"),
               crs = 4326) %>%
  st_transform(crs = crs(b)) %>%
  filter(st_contains(pred.area, ., sparse = FALSE),
         st_contains(nj_outline_dis, ., sparse = FALSE),
         DAY > 120,
         DAY < 197,
         EFFORT_DISTANCE_KM == 0,
         abs(SOLAR_NOON_DIFF) > 5,
         EFFORT_HRS <= 5,
         YEAR > 2007) %>%
  # only points within 4 ft SLR zone
  # Dates between 1 May and 15 July; only stationary counts
  # times > 5 hours from solar noon
  # durations shorter than 5 hours
  # excluding 2005-2007 which had < 20 checklists
  group_by(siteid, YEAR) %>%
  arrange(DAY) %>%
  mutate(visitnum = sample(1:length(siteid)), # randomizes order of visit number
         numvisits = max(visitnum),
         sitepres = as.numeric(max(obs)>0)) %>%
  ungroup() %>%
  arrange(siteid, YEAR, visitnum)

# sf of eBird detection locations
be1.occ.long <- be.occ.long %>%
  filter(sitepres > 0) %>%
  dplyr::select(siteid, YEAR, DAY, time = SOLAR_NOON_DIFF, dur = EFFORT_HRS, visitnum, numvisits, obs) %>%
  filter(visitnum < 11) %>%
  mutate(visitnum_B = paste0("v", visitnum)) %>%
  arrange(siteid, YEAR, DAY) 

# remove 4 ft SLR polygon file from memory
rm(pred.area, nj_outline_dis)

be1.occ.long <- be1.occ.long %>%
  mutate(x = st_coordinates(be1.occ.long)[,1],
         y = st_coordinates(be1.occ.long)[,2])

# sf of eBird non-detection locations
be0.occ.long <- be.occ.long %>%
  filter(sitepres == 0) %>%
  dplyr::select(siteid, YEAR, DAY, time = SOLAR_NOON_DIFF, dur = EFFORT_HRS, visitnum, numvisits, obs) %>%
  filter(visitnum < 11) %>%
  mutate(visitnum_B = paste0("v", visitnum)) %>%
  arrange(siteid, YEAR, visitnum) 

be0.occ.long <- be0.occ.long %>%
  mutate(x = st_coordinates(be0.occ.long)[,1],
         y = st_coordinates(be0.occ.long)[,2])

# put eBird detections into wide format
be1.occ.wide <- expand.grid(
  siteid = unique(be1.occ.long$siteid),
  YEAR = unique(be1.occ.long$YEAR),
  visitnum = 1:10
) %>%
  left_join(be1.occ.long, by = c("siteid", "YEAR", "visitnum")) %>%
  filter(numvisits > 0) %>% # can filter by minimum number of visits here
  pivot_wider(
    id_cols = c(siteid, YEAR, x, y),
    names_from = visitnum,
    values_from = c(obs, DAY, time, dur)
  ) %>%
  filter(is.na(obs_1) == F) %>%
  arrange(siteid, YEAR) %>%
  st_as_sf(coords = c("x", "y"),
           crs = crs(b))
#st_write(be1.occ.wide, "data/shapefiles/ebird_positives.shp") # for use in GIS

# put eBird non-detections into wide format
be0.occ.wide <- expand.grid(
  siteid = unique(be0.occ.long$siteid),
  YEAR = unique(be0.occ.long$YEAR),
  visitnum = 1:10
) %>%
  left_join(be0.occ.long, by = c("siteid", "YEAR", "visitnum")) %>%
  filter(numvisits > 0) %>% # can filter by minimum number of visits here
  pivot_wider(
    id_cols = c(siteid, YEAR, x, y),
    names_from = visitnum,
    values_from = c(obs, DAY, time, dur)
  ) %>%
  filter(is.na(obs_1) == F) %>%
  arrange(siteid, YEAR) %>%
  st_as_sf(coords = c("x", "y"),
           crs = crs(b))
#st_write(be0.occ.wide, "data/shapefiles/ebird_negatives.shp")

# make final eBird data in wide format
be.occ.wide_step1 <-
  bind_rows(be0.occ.wide,  
            be1.occ.wide) %>%
  arrange(siteid, YEAR)

be.occ.wide <- be.occ.wide_step1 %>%
  bind_cols(as.data.frame(extract(b.brick, be.occ.wide_step1))) %>%
  filter(is.na(ss.500) == F)

be.occ.mat <-
  be.occ.wide %>%
  as.data.frame() %>%
  dplyr::select(obs_1:obs_10) %>%
  as.matrix()
# convert to presenece / absence
be.occ.mat = matrix(as.numeric(ifelse(be.occ.mat>0, 1, 0)), ncol = 10)

be.obs.cov.DAY <-
  be.occ.wide %>%
  as.data.frame() %>%
  dplyr::select(DAY_1:DAY_10) %>%
  as.matrix()

be.obs.cov.time <-
  be.occ.wide %>%
  as.data.frame() %>%
  dplyr::select(time_1:time_10) %>%
  as.matrix()

be.obs.cov.dur <-
  be.occ.wide %>%
  as.data.frame() %>%
  dplyr::select(dur_1:dur_10) %>%
  as.matrix()

# convert time to hrs from "solar midnight" format
be.obs.cov.time2 = matrix(as.numeric(ifelse((be.obs.cov.time > 0),
                                            -1 * (12 - be.obs.cov.time),
                                            be.obs.cov.time + 12)), ncol=10)

be.site.cov <-
  be.occ.wide %>%
  as.data.frame() %>%
  dplyr::select(c(siteid, YEAR, ss.500:imp.500))

rm(BLRA_db, be, be.occ.wide_step1)

# Combine both sources of data: ENSP and eBird
# combine detection data
b.occ.mat.ensp_ebird <-
  b.occ.mat %>%
  as.data.frame() %>%
  mutate(v7 = NA,
         e8 = NA,
         e9 = NA,
         e10 = NA) %>%
  as.matrix() %>%
  rbind(be.occ.mat)

# combine observation-level covariates: ENSP and eBird
# ordinal date
b.obs.cov.ord.ensp_ebird <-
  b.obs.cov.ord %>%
  as.data.frame() %>%
  mutate(v7 = NA,
         e8 = NA,
         e9 = NA,
         e10 = NA) %>%
  as.matrix() %>%
  rbind(be.obs.cov.DAY)

# combine observation-level covariates: ENSP and eBird
# duration
ensp.n <- length(b.obs.cov.ord)
b.obs.cov.dur <-
  matrix(rep(0.1666667, ensp.n), ncol = 6)

b.obs.cov.dur.ensp_ebird <- 
  b.obs.cov.dur %>%
  as.data.frame() %>%
  mutate(v7 = NA,
         e8 = NA,
         e9 = NA,
         e10 = NA) %>%
  as.matrix() %>%
  rbind(be.obs.cov.dur)

# a survey-level covariate indicating ensp survey (1) or ebird (0) data
b.obs.cov.SURVEY.ensp_ebird <- matrix(rep(1, dim(b.occ.mat)[1]*10), ncol = 10) %>%
  rbind(matrix(rep(0, dim(be.occ.mat)[1]*10), ncol = 10))

# combine site-level covariates: ENSP and eBird
# year as a factor variable
b.site.cov_temp = b.site.cov %>%
  mutate(YEAR = case_when(year2016 == 1 ~ "2016",
                          year2018 == 1 ~ "2018",
                          year2019 == 1 ~ "2019",
                          TRUE ~ "2015")) %>%
  mutate(yearfac = as.factor(YEAR))

be.site.cov$yearfac <- as.factor(be.site.cov$YEAR)
b.site.cov.ensp_ebird <-
  b.site.cov_temp  %>%
  dplyr::select(yearfac, ss.500:imp.500) %>%
  bind_rows(dplyr::select(be.site.cov, yearfac, ss.500:imp.500)) %>%
  mutate(survey = c(rep(1, nrow(b.occ.mat)), rep(0, nrow(be.occ.mat))))

#########################
#### FORMAT DATA FOR JAGS
#########################

# JAGS data: ENSP data only

# Create design matrix for occupancy covariates
occDM_hm <- model.matrix(~ year2016 + year2018 + year2019 + ss.500 + sharp_hm.500 + sharp_tb.500 + ldev.100 + imp.500, data = b.site.cov)[,-1] # Drop first col.

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
    occDM_hm = as.data.frame(occDM_hm),
    hm.pred = seq(0, 0.7653146, length.out = 50),
    ss.pred = seq(0, 0.6134549, length.out = 50),
    ldev.pred = seq(0, 0.755102, length.out = 50),
    tb.pred = seq(0, 0.5520833, length.out = 50),
    ord.pred = seq(1.19, 1.96, length.out = 50),
    ss.500_fed = filter(owner_covs_data, owner == "Federal", model == "ss.500")[,1],
    ss.500_state = filter(owner_covs_data, owner == "State", model == "ss.500")[,1],
    ss.500_county = filter(owner_covs_data, owner == "County", model == "ss.500")[,1],
    ss.500_mun = filter(owner_covs_data, owner == "Municipal", model == "ss.500")[,1],
    ss.500_nonp = filter(owner_covs_data, owner == "Nonprofit", model == "ss.500")[,1],
    ss.500_npp = filter(owner_covs_data, owner == "Unprotected", model == "ss.500")[,1],
    sharp_hm.500_fed = filter(owner_covs_data, owner == "Federal", model == "sharp_hm.500")[,1],
    sharp_hm.500_state = filter(owner_covs_data, owner == "State", model == "sharp_hm.500")[,1],
    sharp_hm.500_county = filter(owner_covs_data, owner == "County", 
                                 model == "sharp_hm.500")[,1],
    sharp_hm.500_mun = filter(owner_covs_data, owner == "Municipal", 
                              model == "sharp_hm.500")[,1],
    sharp_hm.500_nonp = filter(owner_covs_data, owner == "Nonprofit", 
                               model == "sharp_hm.500")[,1],
    sharp_hm.500_npp = filter(owner_covs_data, owner == "Unprotected", 
                              model == "sharp_hm.500")[,1],
    sharp_tb.500_fed = filter(owner_covs_data, owner == "Federal", model == "sharp_tb.500")[,1],
    sharp_tb.500_state = filter(owner_covs_data, owner == "State", model == "sharp_tb.500")[,1],
    sharp_tb.500_county = filter(owner_covs_data, owner == "County", 
                                 model == "sharp_tb.500")[,1],
    sharp_tb.500_mun = filter(owner_covs_data, owner == "Municipal", 
                              model == "sharp_tb.500")[,1],
    sharp_tb.500_nonp = filter(owner_covs_data, owner == "Nonprofit", 
                               model == "sharp_tb.500")[,1],
    sharp_tb.500_npp = filter(owner_covs_data, owner == "Unprotected", 
                              model == "sharp_tb.500")[,1],
    ldev.100_fed = filter(owner_covs_data, owner == "Federal", model == "ldev.100")[,1],
    ldev.100_state = filter(owner_covs_data, owner == "State", model == "ldev.100")[,1],
    ldev.100_county = filter(owner_covs_data, owner == "County", 
                             model == "ldev.100")[,1],
    ldev.100_mun = filter(owner_covs_data, owner == "Municipal", 
                          model == "ldev.100")[,1],
    ldev.100_nonp = filter(owner_covs_data, owner == "Nonprofit", 
                           model == "ldev.100")[,1],
    ldev.100_npp = filter(owner_covs_data, owner == "Unprotected", 
                          model == "ldev.100")[,1],
    imp.500_fed = filter(owner_covs_data, owner == "Federal", model == "imp.500")[,1],
    imp.500_state = filter(owner_covs_data, owner == "State", model == "imp.500")[,1],
    imp.500_county = filter(owner_covs_data, owner == "County", model == "imp.500")[,1],
    imp.500_mun = filter(owner_covs_data, owner == "Municipal", model == "imp.500")[,1],
    imp.500_nonp = filter(owner_covs_data, owner == "Nonprofit", model == "imp.500")[,1],
    imp.500_npp = filter(owner_covs_data, owner == "Unprotected", model == "imp.500")[,1],
    n.state = nrow(filter(owner_covs_data, owner == "State", model == "ss.500")),
    n.fed = nrow(filter(owner_covs_data, owner == "Federal", model == "ss.500")),
    n.county = nrow(filter(owner_covs_data, owner == "County", model == "ss.500")),
    n.mun = nrow(filter(owner_covs_data, owner == "Municipal", model == "ss.500")),
    n.nonp = nrow(filter(owner_covs_data, owner == "Nonprofit", model == "ss.500")),
    n.npp = nrow(filter(owner_covs_data, owner == "Unprotected", model == "ss.500"))
  )

# JAGS data: eBird data only

# Create design matrix for occupancy covariates
be.site.cov$yearfac <- as.factor(be.site.cov$YEAR)
# design matrix version with SHARP high marsh
occDM_hm <-
  model.matrix( ~ yearfac + ss.500 + sharp_hm.500 + sharp_tb.500 + ldev.100 + imp.500,
                data = be.site.cov)[, -1] # Drop first col.


day_no_NA <- be.obs.cov.DAY / 100
day_no_NA[is.na(day_no_NA)==T] <- 1.47 # set NA values to mean (doesn't affect results)
dur_no_NA <- be.obs.cov.dur
dur_no_NA[is.na(dur_no_NA)==T] <- 0.66 # set NA values to mean (doesn't affect results)
time_no_NA <- be.obs.cov.time2 # time relative to 'solar midnight'
time_no_NA[is.na(time_no_NA)==T] <- 0.78
time_no_NA2 <- time_no_NA^2
y = be.occ.mat

# Bundle and summarize data set
ebird.jags.data <- list(y = y, M = nrow(y), J = ncol(y), ord = day_no_NA, hrs = time_no_NA, hrs2 = time_no_NA2, dur = dur_no_NA, occDM_hm = occDM_hm, 
                        hm.pred = seq(0, 0.7653146, length.out = 50), 
                        hrs.pred = seq(-6.9967, 6.9989, length.out = 50), 
                        ord.pred = seq(1.19, 1.96, length.out = 50),
                        ss.500_fed = filter(owner_covs_data, owner == "Federal", model == "ss.500")[,1],
                        ss.500_state = filter(owner_covs_data, owner == "State", model == "ss.500")[,1],
                        ss.500_county = filter(owner_covs_data, owner == "County", model == "ss.500")[,1],
                        ss.500_mun = filter(owner_covs_data, owner == "Municipal", model == "ss.500")[,1],
                        ss.500_nonp = filter(owner_covs_data, owner == "Nonprofit", model == "ss.500")[,1],
                        ss.500_npp = filter(owner_covs_data, owner == "Unprotected", model == "ss.500")[,1],
                        sharp_hm.500_fed = filter(owner_covs_data, owner == "Federal", model == "sharp_hm.500")[,1],
                        sharp_hm.500_state = filter(owner_covs_data, owner == "State", model == "sharp_hm.500")[,1],
                        sharp_hm.500_county = filter(owner_covs_data, owner == "County", 
                                                     model == "sharp_hm.500")[,1],
                        sharp_hm.500_mun = filter(owner_covs_data, owner == "Municipal", 
                                                  model == "sharp_hm.500")[,1],
                        sharp_hm.500_nonp = filter(owner_covs_data, owner == "Nonprofit", 
                                                   model == "sharp_hm.500")[,1],
                        sharp_hm.500_npp = filter(owner_covs_data, owner == "Unprotected", 
                                                  model == "sharp_hm.500")[,1],
                        sharp_tb.500_fed = filter(owner_covs_data, owner == "Federal", model == "sharp_tb.500")[,1],
                        sharp_tb.500_state = filter(owner_covs_data, owner == "State", model == "sharp_tb.500")[,1],
                        sharp_tb.500_county = filter(owner_covs_data, owner == "County", 
                                                     model == "sharp_tb.500")[,1],
                        sharp_tb.500_mun = filter(owner_covs_data, owner == "Municipal", 
                                                  model == "sharp_tb.500")[,1],
                        sharp_tb.500_nonp = filter(owner_covs_data, owner == "Nonprofit", 
                                                   model == "sharp_tb.500")[,1],
                        sharp_tb.500_npp = filter(owner_covs_data, owner == "Unprotected", 
                                                  model == "sharp_tb.500")[,1],
                        ldev.100_fed = filter(owner_covs_data, owner == "Federal", model == "ldev.100")[,1],
                        ldev.100_state = filter(owner_covs_data, owner == "State", model == "ldev.100")[,1],
                        ldev.100_county = filter(owner_covs_data, owner == "County", 
                                                 model == "ldev.100")[,1],
                        ldev.100_mun = filter(owner_covs_data, owner == "Municipal", 
                                              model == "ldev.100")[,1],
                        ldev.100_nonp = filter(owner_covs_data, owner == "Nonprofit", 
                                               model == "ldev.100")[,1],
                        ldev.100_npp = filter(owner_covs_data, owner == "Unprotected", 
                                              model == "ldev.100")[,1],
                        imp.500_fed = filter(owner_covs_data, owner == "Federal", model == "imp.500")[,1],
                        imp.500_state = filter(owner_covs_data, owner == "State", model == "imp.500")[,1],
                        imp.500_county = filter(owner_covs_data, owner == "County", model == "imp.500")[,1],
                        imp.500_mun = filter(owner_covs_data, owner == "Municipal", model == "imp.500")[,1],
                        imp.500_nonp = filter(owner_covs_data, owner == "Nonprofit", model == "imp.500")[,1],
                        imp.500_npp = filter(owner_covs_data, owner == "Unprotected", model == "imp.500")[,1],
                        n.state = nrow(filter(owner_covs_data, owner == "State", model == "ss.500")),
                        n.fed = nrow(filter(owner_covs_data, owner == "Federal", model == "ss.500")),
                        n.county = nrow(filter(owner_covs_data, owner == "County", model == "ss.500")),
                        n.mun = nrow(filter(owner_covs_data, owner == "Municipal", model == "ss.500")),
                        n.nonp = nrow(filter(owner_covs_data, owner == "Nonprofit", model == "ss.500")),
                        n.npp = nrow(filter(owner_covs_data, owner == "Unprotected", model == "ss.500")))

# ENSP & eBird data combined

# Create design matrix for occupancy covariates - version with SHARP high marsh
occDM_hm <- model.matrix(~ yearfac + ss.500 + sharp_hm.500 + sharp_tb.500 + ldev.100 + imp.500, data = b.site.cov.ensp_ebird)[,-1] # Drop first col.

day_no_NA <- b.obs.cov.ord.ensp_ebird / 100
day_no_NA[is.na(day_no_NA)] <- 1.52 # set NA values to mean (doesn't affect results as there are no NA dates wite presence/absence data)
dur_no_NA <- b.obs.cov.dur.ensp_ebird
dur_no_NA[is.na(dur_no_NA)] <- 0.41 # set NA values to mean (doesn't affect results as there are no NA dates wite presence/absence data)
survey.obs <- b.obs.cov.SURVEY.ensp_ebird
time_no_NA <- be.obs.cov.time 
time_no_NA[is.na(time_no_NA)==T] <- -0.37
time_no_NA = time_no_NA/10
time_no_NA2 <- time_no_NA^2
y = b.occ.mat.ensp_ebird
# Bundle and summarize data set
ensp_ebird.jags.data <- list(y = y, M = nrow(y), J = ncol(y), ord = day_no_NA, survey.obs = survey.obs, dur = dur_no_NA, 
       occDM_hm = as.data.frame(occDM_hm), 
       hm.pred = seq(0, 0.7653146, length.out = 50), 
       ss.pred = seq(0, 0.6134549, length.out = 50),
       tb.pred = seq(0, 0.5520833, length.out = 50),
       ldev.pred = seq(0, 0.755102, length.out = 50),
       ord.pred = seq(1.19, 1.96, length.out = 50),
       dur.pred = seq(0.001, 5, length.out = 50),
       ss.500_fed = filter(owner_covs_data, owner == "Federal", model == "ss.500")[,1],
       ss.500_state = filter(owner_covs_data, owner == "State", model == "ss.500")[,1],
       ss.500_county = filter(owner_covs_data, owner == "County", model == "ss.500")[,1],
       ss.500_mun = filter(owner_covs_data, owner == "Municipal", model == "ss.500")[,1],
       ss.500_nonp = filter(owner_covs_data, owner == "Nonprofit", model == "ss.500")[,1],
       ss.500_npp = filter(owner_covs_data, owner == "Unprotected", model == "ss.500")[,1],
       sharp_hm.500_fed = filter(owner_covs_data, owner == "Federal", model == "sharp_hm.500")[,1],
       sharp_hm.500_state = filter(owner_covs_data, owner == "State", model == "sharp_hm.500")[,1],
       sharp_hm.500_county = filter(owner_covs_data, owner == "County", 
                                    model == "sharp_hm.500")[,1],
       sharp_hm.500_mun = filter(owner_covs_data, owner == "Municipal", 
                                 model == "sharp_hm.500")[,1],
       sharp_hm.500_nonp = filter(owner_covs_data, owner == "Nonprofit", 
                                  model == "sharp_hm.500")[,1],
       sharp_hm.500_npp = filter(owner_covs_data, owner == "Unprotected", 
                                 model == "sharp_hm.500")[,1],
       sharp_tb.500_fed = filter(owner_covs_data, owner == "Federal", model == "sharp_tb.500")[,1],
       sharp_tb.500_state = filter(owner_covs_data, owner == "State", model == "sharp_tb.500")[,1],
       sharp_tb.500_county = filter(owner_covs_data, owner == "County", 
                                    model == "sharp_tb.500")[,1],
       sharp_tb.500_mun = filter(owner_covs_data, owner == "Municipal", 
                                 model == "sharp_tb.500")[,1],
       sharp_tb.500_nonp = filter(owner_covs_data, owner == "Nonprofit", 
                                  model == "sharp_tb.500")[,1],
       sharp_tb.500_npp = filter(owner_covs_data, owner == "Unprotected", 
                                 model == "sharp_tb.500")[,1],
       ldev.100_fed = filter(owner_covs_data, owner == "Federal", model == "ldev.100")[,1],
       ldev.100_state = filter(owner_covs_data, owner == "State", model == "ldev.100")[,1],
       ldev.100_county = filter(owner_covs_data, owner == "County", 
                                model == "ldev.100")[,1],
       ldev.100_mun = filter(owner_covs_data, owner == "Municipal", 
                             model == "ldev.100")[,1],
       ldev.100_nonp = filter(owner_covs_data, owner == "Nonprofit", 
                              model == "ldev.100")[,1],
       ldev.100_npp = filter(owner_covs_data, owner == "Unprotected", 
                             model == "ldev.100")[,1],
       imp.500_fed = filter(owner_covs_data, owner == "Federal", model == "imp.500")[,1],
       imp.500_state = filter(owner_covs_data, owner == "State", model == "imp.500")[,1],
       imp.500_county = filter(owner_covs_data, owner == "County", model == "imp.500")[,1],
       imp.500_mun = filter(owner_covs_data, owner == "Municipal", model == "imp.500")[,1],
       imp.500_nonp = filter(owner_covs_data, owner == "Nonprofit", model == "imp.500")[,1],
       imp.500_npp = filter(owner_covs_data, owner == "Unprotected", model == "imp.500")[,1],
       n.state = nrow(filter(owner_covs_data, owner == "State", model == "ss.500")),
       n.fed = nrow(filter(owner_covs_data, owner == "Federal", model == "ss.500")),
       n.county = nrow(filter(owner_covs_data, owner == "County", model == "ss.500")),
       n.mun = nrow(filter(owner_covs_data, owner == "Municipal", model == "ss.500")),
       n.nonp = nrow(filter(owner_covs_data, owner == "Nonprofit", model == "ss.500")),
       n.npp = nrow(filter(owner_covs_data, owner == "Unprotected", model == "ss.500"))
)
