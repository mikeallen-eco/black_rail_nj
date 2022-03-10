# function to create moving window of 
# land cover % for each cell in NWI marsh raster
pland.nwi = function(raster, 
                 code, 
                 radius) {
  # create the circular buffers (as matrices for the focal function)
  circles <- focalWeight(raster, radius, type = 'circle')
  circles[circles > 0] <- 1
  
  prop = function(x) {
    (sum(x == code) / sum(x > 0))
  }
  
  raster[is.na(raster[])] <- 1000
  
  pland.map = focal(
    raster,
    w = circles,
    fun = prop,
    pad = T,
    NAonly = F
  )
  
  return(pland.map)
}


# function to create moving window of 
# land cover % for each cell in SHARP marsh classification raster
# uses mean because cells currently contain proportions
pland.sharp = function(raster, 
                       radius) {
  # create the circular buffers (as matrices for the focal function)
  circles <- focalWeight(raster, radius, type = 'circle')
  circles[circles > 0] <- 1
  
  raster[is.na(raster[])] <- 0
  
  pland.map = focal(
    raster,
    w = circles,
    fun = mean,
    pad = T,
    NAonly = F
  )
  
  return(pland.map)
}

