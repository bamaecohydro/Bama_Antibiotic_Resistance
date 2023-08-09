#libraries
library(nhdplusTools)
library(sf)
library(mapview)

#Create starting point
start_point <- st_sfc(st_point(c(-89.362239, 43.090266)), crs = 4269)
start_comid <- discover_nhdplus_id(start_point)
start_comid

#Snag flowline
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

#plot for funzies
mapview(start_point) + mapview(flowline)

#get nhdplus files
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)
#Snag catchment
catchment <- sf::read_sf(subset_file, "CatchmentSP")
mapview(start_point) + mapview(flowline) + mapview(catchment)
