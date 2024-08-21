#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: 'Bama Physiographic Region Map
# Coder: Nate Jones
# Date: 8/18/2024
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup workspace ---------------------------------------------------------------
#Load packages of interest
library(spData)
library(tidyverse)
library(sf)
library(tmap)

#load shapes
al <- us_states[1,]
physio <- st_read("data//physio.shp")

#Prep shapes -------------------------------------------------------------------
physio_sub <- physio %>% as.data.frame() %>% dplyr::select(FENCODE, DIVISION, PROVINCE, SECTION, PHYSIODD_I) %>% 
  filter(FENCODE == "4a" |
           FENCODE == "8f" |
           FENCODE == "3d" |
           FENCODE == "6a" |
           FENCODE == "11a")
physio_sf <- left_join(physio_sub, physio) %>% 
  st_as_sf()
physio_utm <- physio_sf %>% st_transform("+proj=utm +zone=16 +datum=WGS84")
physio_utm <- st_buffer(physio_utm, dist=0)
al_utm <- al %>% st_transform("+proj=utm +zone=16 +datum=WGS84")
physio_al <- st_intersection(physio_utm, al_utm)


#Plot --------------------------------------------------------------------------
tm_shape(al) +
  tm_borders(col = "black") +
  tm_shape(physio_al) +
  tm_fill(
    col = "FENCODE", 
    palette = c("grey20", "grey60", "grey30", "grey40", "grey50"), 
    alpha = 0.75, 
    border.col = "black", 
    legend.show = F, 
    legend.z = 100) +
  tm_shape(physio_al) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_shape(al) +
  tm_borders(col = "black", lwd = 3) +
  tm_compass(text.size = 1.2, position = c(0.85, 0.04)) +
  tm_scale_bar(text.size = 1.1, position = c(0.33, 0.05), width = 0.4) 
alabama_physio
tmap_save(alabama_physio, filename = "docs/Alabama_physiographic_regions.png", width = 4, height = 6.5, units = "in")
