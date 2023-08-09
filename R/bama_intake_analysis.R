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

#define scratch directory
scratch_dir <- "C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\scratch\\"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 clip pwi to nhd_network --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Tidy Data -----------------------------------------------------------------
#Load data of interet
pwi <- st_read("C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\EPA_WaterIntakes\\estimated_surface_water_intakes.shp")
subwatershed <- st_read("C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\NHD\\NHDPlusSA\\WBD_Subwatershed.shp")
fac_3e <- raster("C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\NHD\\NHDPlusSA\\NHDPlusFdrFac03e\\fac")
fac_3f <- raster("C:\\WorkspaceR\\Bama_Antibiotic_Resistance\\data\\NHD\\NHDPlusSA\\NHDPlusFdrFac03f\\fac")

#repoject subwatershed
subwatershed <- st_transform(subwatershed, crs = st_crs(pwi))

#Clip pwi to subwatershed area
pwi <- pwi[subwatershed, ]

#Combine 3e and 3f, and then reproject
fac <- merge(fac_3e, fac_3f)
fac <- projectRaster(fac, crs=crs(pwi))

#export data to scratch folder
st_write(pwi, paste0(scratch_dir, "pwi.shp"))
writeRaster(fac, paste0(scratch_dir, "fac.tif"))


