# make function to extract values from rasters and format into a data frame
getval_PA <- function(raster_name, PA_layer, ownertype, datatype, valuetype = NULL){
  data = extract(raster_name, PA_layer)[[1]] %>%
    as.data.frame() %>%
    rename(value = 1) %>%
    filter(is.na(value)==F) %>%
    mutate(owner = ownertype,
           model = datatype,
           type = valuetype)
  return(data)
}
