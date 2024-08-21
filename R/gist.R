#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Quick Watershed Delineation using NHDPlusTools
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 8/114/2023
# Purpose: Watershed Delineation using NHDPlust Tools
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup Environment --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear memory
remove(list=ls())

#Call relevant libraries
library(tidyverse)
library(mapview)
library(raster)
library(sf)
library(tigris)

#Load Alabama Shape
bama <- states() %>% dplyr::filter((STUSPS %in% c('AL')))

#Cahaba River Outlet
cahaba_outlet<- st_sfc(st_point(c(-87.125258,  32.335792)), crs = 4269)

#Rerpoject and clip to continental US
bama <- st_transform(bama, 5070)
cahaba_outlet <- st_transform(cahaba_outlet, 5070) 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Delineate ----------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create starting point
start_comid <- discover_nhdplus_id(cahaba_outlet)
start_comid

#Snag flowline
flowlines <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

#plot for funzies
mapview(cahaba_outlet) + mapview(flowline)

# Note, the next step pulls NHDplus data from NLNDI server. For large watersheds, 
# it will likely be faster to download the WBD form the USGS National Map website (link below), 
# and intersect the "fowlines" shape above with the downloaded layer.
# https://www.usgs.gov/national-hydrography/access-national-hydrography-products

#get nhdplus files
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowlines$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)
#Snag catchment files
catchment <- sf::read_sf(subset_file, "CatchmentSP")

#Dissovle files to single catchment
catchment <- sf::st_union(catchment)

#Plot for funzies!
mapview(start_point) + mapview(catchment)
