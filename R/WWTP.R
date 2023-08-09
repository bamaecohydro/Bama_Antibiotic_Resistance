#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Map WWTP and Water Intakes
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 4/17/2021
# Purpose: Download SDWIS data from Envirofacts Data Service API
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Data Sources: 
#   Sample of Surface Water Intake Data (TNC): https://water.nature.org/waterblueprint
#   Sample of WWTP data (HydroWaste): https://www.hydrosheds.org/products/hydrowaste

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
pwi_point <- st_read("data//wat_012_city_water_intakes//CWM_v2_2//Snapped_Withdrawal_Points.shp")
pwi_shed  <- st_read("data//wat_012_city_water_intakes//CWM_v2_2//World_Watershed8.shp")

#Load Waste Water Treatement Plant data
wwtp <- 
  read_csv("data//HydroWASTE_v10.csv") %>% 
  st_as_sf(
    coords = c("LON_OUT", "LAT_OUT"), 
    crs = '+proj=longlat +datum=WGS84 +no_defs')

#Load US data
states <- states() %>% dplyr::filter(!(STUSPS %in% c('HI','VI','MP', 'GU', 'AK', 'AS', 'PR')))

#Rerpoject and clip to continental US
states <- st_transform(states, 5070)
pwi_point <- st_transform(pwi_point, 5070) 
pwi_point <- pwi_point[states,]
pwi_shed <- st_transform(pwi_shed, 5070) 
pwi_shed <- pwi_shed[states,]
wwtp <- st_transform(wwtp, 5070)
wwtp <- wwtp[states,]

#plot for funzies
states %>% st_geometry() %>% plot()
pwi_point %>% st_geometry() %>% plot(., add=T, col="blue", pch=19)
wwtp %>% st_geometry() %>% plot(., add=T, col="brown", pch=19, cex=0.1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Estimate number of WWTP per pwi watershed --------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#create function
fun<-function(n){
  # Identify unique id
  id <- pwi_shed$DVSN_ID[n]
  
  # Identify shed
  shed <- pwi_shed %>% filter(DVSN_ID == id)
  
  #estimate area
  shed_area_km2 <- as.numeric(paste(st_area(shed)))/1000000
  
  # identify wwtps in shed
  wwtps <- wwtp[shed,]
  
  # count wwpt
  n_wwtps <- nrow(wwtps)
  
  # density of wwpt
  d_wwtps <- n_wwtps/shed_area_km2
  
  #export results
  tibble(id, n_wwtps, shed_area_km2, d_wwtps)
}

#apply function 
output <- lapply(
    X = seq(1, nrow(pwi_shed)), #
    FUN = fun) %>% 
  bind_rows()

#join to table
pwi_point <- pwi_point %>% left_join(., output %>% rename(DVSN_ID = id))
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Plots --------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.1 Density map --------------------------------------------------------------
#prep pwi_point data
pwi_point <- pwi_point %>% 
  mutate(
    lat = st_coordinates(pwi_point)[,1], 
    lon = st_coordinates(pwi_point)[,2],
    d_wwtps_100 = d_wwtps*100) %>% 
  filter(d_wwtps_100<1)

#Create density map
density_map <- pwi_point %>%
  arrange(d_wwtps) %>% 
  ggplot()+
    geom_sf(data = states, lwd=0.5) + 
    geom_point(
      aes(
        x=lat,
        y=lon,
        color = d_wwtps_100, 
        size =  d_wwtps_100, 
        alpha = d_wwtps_100, 
      #shape= NP
    )) +
    scale_color_distiller(
      palette = 'Spectral',
      direction = -1, 
      breaks = seq(0,1,25)) +
  scale_size_continuous(
    range = c(0.5, 3.5), 
    breaks = seq(0,1,25)) +
  scale_alpha_continuous(
    range = c(0.5,.8) , 
    breaks = seq(0,1,25)) +
  #scale_shape(labels = c("Perennial", "Non-perennial")) +
  guides(
    color = guide_colorbar(
      title ="WWTP Density", 
      order = 1),
    size="none", 
    alpha = "none"
  ) +
  theme_bw() +
  theme(
    legend.position = 'right',
    legend.justification = "center") +
  xlab(NULL) +
  ylab(NULL) 

density_map

# 3.2 Histogram ----------------------------------------------------------------
pwi_point$d_wwpts_cut <- cut(pwi_point$d_wwtps_100, c(-1, .001, 0.1, 0.25, 0.5, 1)) #create flow categorices

pwi_density <- pwi_point %>% 
  ggplot(aes(x=d_wwpts_cut)) +
    geom_bar() +
    scale_x_discrete(
      name = "Upstream WWTP Density [WWTP/km^2]", 
      labels = c("0", "0-0.1", "0.1-1.25", "0.25-0.5",">0.5")) +
    scale_y_continuous(name = "Number of Drinking\nWater Intakes") +
    theme_classic() +
    theme(
      axis.title = element_text(size = 14), 
      axis.text  = element_text(size = 10)
    )

# 3.2 Bama ---------------------------------------------------------------------
#Create bama shape
bama_shp <- states() %>% filter(STUSPS == "AL") %>% st_transform(., 5070)

#Subset shapes to bama
wwtp_bama <- wwtp[bama_shp,]
pwi_bama <- pwi_point[bama_shp,]

#PWI Sheds
pwi_sheds_bama <- pwi_shed %>% filter(DVSN_ID %in% pwi_bama$DVSN_ID)
pwi_sheds_bama <- st_crop(pwi_sheds_bama, bama_shp)

#Add coords to wwtp_bama
wwtp_bama <- wwtp_bama %>% 
  mutate(
    lat = st_coordinates(wwtp_bama)[,1], 
    lon = st_coordinates(wwtp_bama)[,2])


#Create bama map
#Create density map
bama_map<-bama_shp %>%
  ggplot()+
  geom_sf(data = bama_shp, lwd=0.5) + 
  geom_sf(data = pwi_sheds_bama, lwd=0.5, bg="light blue")+
  geom_point(
    data= wwtp_bama, 
    aes(x = lat, y = lon), 
    pch = 19, 
    cex = 1, 
    col = "brown", 
    alpha = 0.6) + 
  geom_point(
    data = pwi_bama,
    aes(x=lat,y=lon),
    pch = 19,
    cex = 4, 
    color = "dark blue") +
  theme_bw() +
  theme(
    legend.position = 'none',
    legend.justification = "center") +
  xlab(NULL) +
  ylab(NULL) 


# 3.3 Print! -------------------------------------------------------------------

#Write shape
st_write(bama_shp, "C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\surface_water_intakes\\bama.shp")

#Plot
ggsave(plot = pwi_density, file = 'docs/density.png', width = 4.25, height = 3, units = "in", dpi = 300)
ggsave(plot = density_map, file = "docs/density_map.png", width = 6, height = 3.75, units="in", dpi = 300)
ggsave(plot = bama_map, file = "docs/bama_map.png", width = 3, height = 4, units="in", dpi = 300)
