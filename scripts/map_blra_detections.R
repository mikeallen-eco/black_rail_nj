library(rnaturalearth)
library(cowplot)

# read in US boundary from rnaturalearth for mapping
world <-
  ne_countries(country = 'united states of america',
               scale = 'medium',
               returnclass = "sf")

# tidal, emergent marsh (not permanently flooded) from NWI to mask rasters
# marsh.mask = filter(
#       nwi,
#       SYSTEM_NAME == "Estuarine",
#       SUBSYSTEM_NAME == "Intertidal",
#       CLASS_NAME == "Emergent",
#       WATER_REGIME_NAME != "Permanently Flooded"
#     ) %>%
#   mutate(habitat = "habitat") %>%
#   dplyr::select(habitat) %>%
#   group_by(habitat) %>%
#   summarise()
marsh.mask <-
  read_sf("data/big_files/nwi_tidal_marsh.shp") # to save time

ggplot() +
  geom_sf(data = st_geometry(world),
          color = "transparent",
          fill = "gray") +
  geom_sf(data = st_geometry(marsh.mask), color = "olivedrab") +
  geom_sf(data = st_geometry(filter(b, pres == 0)),
          color = "black",
          size = 1) +
  geom_sf(data = st_geometry(filter(b, pres == 1)),
          color = "yellow",
          size = 1) +
  geom_sf(data = st_geometry(be.occ.wide), 
          color = "gray", 
          size = 1, 
          alpha = 0.5) +
  geom_sf(data = st_geometry(be1.occ.wide), 
          color = "red", 
          size = 1, 
          alpha = 0.5) +  
  coord_sf(
    xlim = c(-75.75,-73.75),
    ylim = c(38.8, 41),
    expand = FALSE
  ) +
  theme_cowplot() +
  theme(panel.background = element_rect(fill = 'skyblue', colour = "transparent"))
#ggsave("figures/blra_map.jpg", width = 5, height = 7)