#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Map WWTP and Water Intakes
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 4/17/2021
# Purpose: Download SDWIS data from Envirofacts Data Service API
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

#Load Public Water Intake Spatial Data
pwi_point <- st_read("data//EPA_WaterIntakes//estimated_surface_water_intakes.shp")

#Load Waste Water Treatement Plant data
wwtp <- 
  read_csv("data//HydroWaste//HydroWASTE_v10.csv") %>% 
  st_as_sf(
    coords = c("LON_OUT", "LAT_OUT"), 
    crs = '+proj=longlat +datum=WGS84 +no_defs')

#Load US data
states <- states() %>% dplyr::filter((STUSPS %in% c('AL')))

#Rerpoject and clip to continental US
states <- st_transform(states, 5070)
pwi_point <- st_transform(pwi_point, 5070) 
pwi_point <- pwi_point[states,]
wwtp <- st_transform(wwtp, 5070)
wwtp <- wwtp[states,]

#plot for funzies
states %>% st_geometry() %>% plot()
pwi_point %>% st_geometry() %>% plot(., add=T, col="blue", pch=19)
wwtp %>% st_geometry() %>% plot(., add=T, col="brown", pch=19, cex=0.1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Identify study watersheds ------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Coosa -------------------------------------------------------------------------
start_point <- st_sfc(st_point(c(-86.229824,  32.523362)), crs = 4269)
start_comid <- discover_nhdplus_id(start_point)
start_comid

#Snag flowline
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000) 
flowline <- flowline$UT_flowlines 
flowline <- flowline %>% st_transform(., 5070)

#plot for funzies
mapview(start_point) + mapview(flowline)

#get nhdplus files
sheds <- st_read("data//USGS_WBD//shape//WBDHU12.shp") %>% st_transform(., 5070)

#Snag catchment
coosa_catchment <- sheds[flowline,]
coosa_catchment <- sf::st_union(coosa_catchment)
mapview(coosa_catchment)

#Black Warrior River -----------------------------------------------------------
start_point <- st_sfc(st_point(c(-87.767154,  32.568206)), crs = 4269)
start_comid <- discover_nhdplus_id(start_point)
start_comid

#Snag flowline
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)
flowline <- flowline$UT_flowlines 
flowline <- flowline %>% st_transform(., 5070)

#plot for funzies
mapview(start_point) + mapview(flowline)

#Snag catchment
bw_catchment <- sheds[flowline,]
bw_catchment <- sf::st_union(bw_catchment)
mapview(bw_catchment)

#Cahaba -------------------------------------------------------------------------
start_point <- st_sfc(st_point(c(-87.125258,  32.335792)), crs = 4269)
start_comid <- discover_nhdplus_id(start_point)
start_comid

#Snag flowline
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)
flowline <- flowline$UT_flowlines 
flowline <- flowline %>% st_transform(., 5070)

#plot for funzies
mapview(start_point) + mapview(flowline)

#Snag catchment
cahaba_catchment <- sheds[flowline,]
cahaba_catchment <- sf::st_union(cahaba_catchment)
mapview(cahaba_catchment)

#Cahaba -------------------------------------------------------------------------
start_point <- st_sfc(st_point(c(-87.125258,  32.335792)), crs = 4269)
start_comid <- discover_nhdplus_id(start_point)
start_comid

#Snag flowline
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)
flowline <- flowline$UT_flowlines 
flowline <- flowline %>% st_transform(., 5070)

#plot for funzies
mapview(start_point) + mapview(flowline)

#Snag catchment
cahaba_catchment <- sheds[flowline,]
cahaba_catchment <- sf::st_union(cahaba_catchment)
mapview(cahaba_catchment)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Create map of study watersheds -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create bama shape
bama_shp <- states() %>% filter(STUSPS == "AL") %>% st_transform(., 5070)

#Add xy points to shp files
wwtp <- wwtp %>% 
  mutate(
    lat = st_coordinates(wwtp)[,1], 
    lon = st_coordinates(wwtp)[,2])
pwi_point <- pwi_point %>% 
  mutate(
    lat = st_coordinates(pwi_point)[,1], 
    lon = st_coordinates(pwi_point)[,2])


#Create bama map
bama_shp %>%
  ggplot()+
  geom_sf(data = bama_shp, lwd=0.5) + 
  geom_sf(data = bw_catchment, lwd=0.5, bg="light blue")+
  geom_sf(data = cahaba_catchment, lwd=0.5, bg="light blue")+
  geom_sf(data = coosa_catchment, lwd=0.5, bg="light blue")+
  geom_point(
    data= wwtp, 
    aes(x = lat, y = lon), 
    pch = 19, 
    cex = 0.75, 
    col = "brown", 
    alpha = 0.6) + 
  geom_point(
    data = pwi_point,
    aes(x=lat,y=lon),
    pch = 19,
    cex = 1.2, 
    alpha = 0.5,
    color = "dark blue") +
  theme_bw() +
  theme(
    legend.position = 'none',
    legend.justification = "center") +
  xlab(NULL) +
  ylab(NULL) 

# 3.3 Print! -------------------------------------------------------------------
ggsave(file = "docs/bama_map.png", width = 3, height = 4, units="in", dpi = 300)


