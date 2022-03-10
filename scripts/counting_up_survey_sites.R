# counting eBird
be.occ.counting <- st_drop_geometry(be.occ.wide)[,1:12]
pres.during.year <- apply(be.occ.counting[,3:12], 1L, max, na.rm = T)
pres.during.year <- as.numeric(ifelse(pres.during.year > 1, 1, pres.during.year))
be.occ.count2 <- data.frame(be.occ.counting[1:2], pres.during.year = pres.during.year)

# How many times were rails detected at the same point in multiple years 
count.years.pres.per.site <- be.occ.count2 %>% 
  group_by(siteid) %>%
  summarize(num.year.pres = sum(pres.during.year)) %>%
  arrange(desc(num.year.pres))

# count number of years each site was surveyed
table(table(be.occ.count2$siteid))
#   1   2   3   4   5   6   7   8   9  10  11  12 
# 752  94  50  29  15  10   8   6   6   3   2   2 

# how many surveyed in multiple years
94 + 50 + 29 + 15 + 10 +  8 +  6 +  6 +  3 +  2 +  2
# [1] 225
225/977
# [1] 0.2302968

# detections and non-detections by year
table(be.occ.count2$YEAR, be.occ.count2$pres.during.year)
#        0   1
# 2008  37   1
# 2009  37   3
# 2010  63   1
# 2011  62   1
# 2012  82   0
# 2013 117   0
# 2014 121   1
# 2015 169   3
# 2016 197   0
# 2017 191   2
# 2018 207   1
# 2019 279   0

#####################
# counting survey data
#####################

b.occ.counting <- b.occ.wide[,1:9]
pres.during.year.survey <- apply(b.occ.counting[,4:9], 1L, max, na.rm = T)
pres.during.year.survey <- as.numeric(ifelse(pres.during.year.survey > 1, 1, pres.during.year.survey))
b.occ.count2 <- data.frame(b.occ.counting[1:2], pres.during.year = pres.during.year.survey)
table(b.occ.count2$year, b.occ.count2$pres.during.year)

# count number of years each site was surveyed
table(table(b.occ.count2$pointid))
# 1   2   3   4 
# 261  99  11   2 

# how many surveyed more than once?
99  + 11  +  2  # = 112/373 = 0.3002

# How many times were rails detected at the same point in multiple years 
# (only one)
count.years.pres.per.site.ensp <- b.occ.count2 %>% 
  group_by(pointid) %>%
  summarize(num.year.pres = sum(pres.during.year)) %>%
  arrange(desc(num.year.pres))

# number of survey points in 2016 with duration = 15
test = b.clip %>%
  filter(year == 2016, duration_survey > 10)

length(unique(test2$pointid)) # 140

# mean number of visits per point
visits_per_pt <- b.clip %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  group_by(pointid.year) %>% 
  summarise(count = length(survey))

mean(visits_per_pt$count) # 2.9

# count total number of eBird checklists in zone before filtering
be.tally <-
  sf::st_as_sf(be,
               coords = c("LONGITUDE", "LATITUDE"),
               crs = 4326) %>%
  st_transform(crs = crs(b)) %>%
  filter(st_contains(pred.area, ., sparse = FALSE),
         st_contains(nj_outline_dis, ., sparse = FALSE))

nrow(be.tally) # 194260 checklists
length(unique(be.tally$siteid)) # 36701 locations
