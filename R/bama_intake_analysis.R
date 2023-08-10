#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Bama SW Intake Analysis
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 8/9/2023
# Purpose: Estimate sources of antibacterial resistance to drining water intakes
#          accross Alabama. (Beta analysis for collaborative EPA grant)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Step 1: clip SW intake points to NHD
#Step 2: Delineate watersheds
#Step 3: Estimate # of CAFOs, WWTPs, and septics for each surface water intake

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup workspace ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list = ls())

#load packages of interest
library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(mapview)
library(nhdplusTools)
library(riverdist)
library(tigris)
library(fasterize)

#define scratch directory
scratch_dir <- "C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\scratch\\"

#Alabama State
states <- states() %>% 
  dplyr::filter(STUSPS == "AL") %>% 
  st_transform(., crs = 4269)

#public water intakes
pwi <- st_read("C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\EPA_WaterIntakes\\estimated_surface_water_intakes.shp") %>% 
  st_transform(., crs = 4269)

#Black Warrior AFO/CAFO (https://www.google.com/maps/d/viewer?mid=1bIRTEfDjKj2wplmwj2vRMzcKJ3If8VPK&ll=33.16171093100135%2C-87.82940270239635&z=11)
cafo <- st_read('C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\BWR_AFO_CAFO\\afo_cafo.shp') %>% 
  st_transform(., crs = 4269)

#USGS Well Locations (proxy for septics)
septics <- raster("C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\USGS_Well_Locations\\REM_map_2010.tif")

#WWTP from HydroSheds
wwtp <- 
  read_csv("C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\HydroWaste\\HydroWaste_v10.csv") %>% 
  st_as_sf(
    coords = c("LON_OUT", "LAT_OUT"), 
    crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  st_transform(., crs = 4269)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Delineate watershed for each pwi -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function to retrieve data from each PWI's watershed
fun <- function(n){

#Identify pwi of interest
start_point <- pwi[n,]

#Identify NHD reach 
start_comid <- discover_nhdplus_id(start_point, raindrop = T)
start_comid

#Snag flowline
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid$comid[1]), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

#get nhdplus files
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = 'download', 
                         flowline_only = FALSE,
                         return_data = TRUE, 
                         overwrite = TRUE)
#Identify catchment
shed <- sf::read_sf(subset_file, "CatchmentSP")
shed <- st_union(shed)

#Estimate number of wwtp
n_wwtp <- wwtp[shed,]
n_wwtp <- nrow(n_wwtp)

#Estiamte number of afo/cafo
n_cafo <- cafo[shed,]
n_cafo <- nrow(n_cafo)

#Estimate number of septics
shed_proj <- st_transform(shed, crs = st_crs(septics@crs))
mask <- fasterize(st_as_sf(shed_proj), septics)
n_setpics <- crop(septics, mask)
n_septics <- raster::mask(septics, mask)
n_septics <- cellStats(n_septics, sum, na.rm=T)

#Estimate Area
area_km2 <- shed %>% st_area() %>% paste() %>% as.numeric()
area_km2 <- area_km2/1000000

#Export
tibble(
  n, 
  comid = start_comid$comid[1], 
  n_wwtp, 
  n_septics, 
  n_cafo, 
  area_km2)
}

#Create error function
error_fun <- function(n){
  tryCatch(
    fun(n), 
    error = function(e) tibble(n,comid = -9999, n_wwtp = -9999, n_septics = -9999, n_cafo = -9999, area_km2 = -9999))
}

#apply error function
output <- lapply(
  X = seq(1, nrow(pwi)), 
  FUN = error_fun)

#bind rows
df <- bind_rows(output)
df

