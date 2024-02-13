# Mapping Black Rail habitat in New Jersey
Code to model black rail occurrence in NJ from the NJ SeaGrant-funded project: "Mapping eastern black rail habitat to aid coastal conservation and climate resiliency planning". This code underpins the paper that was published in 2023: "Integrating habitat models for threatened species with landownership information to inform coastal resiliency and conservation planning". The paper can be found here:
https://doi.org/10.1017/S037689292200039X

Mapped SDM results are located in the output folder of this GitHub repository and are as follows:

1. The file "ensp_ebird_cell_pred_dur_med.tif" contains the point estimates (i.e., posterior medians) for black rail occupancy probability in 860,298 grid cells (30 x 30 m) corresponding to all emergent tidal marsh in the state as mapped by the USFWS National Wetlands Inventory (~774 square kilometers). The cell size, origin, and projection of this GeoTiff raster file match the USGS GAP/LANDFIRE National Terrestrial Ecosystems data set.

2. The file "ensp_ebird_cell_pred_dur_CIwidth.tif"" contains the 95% credible interval width for all cells.

3. The jpg file shows these two maps graphically along with the ownership data we compiled.


