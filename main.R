

# Set working directory
setwd("Documents/manhattan-froyo-map")


# Load packages
library(dplyr)
library(readr)
library(proj4)
library(httr)
library(jsonlite)
library(data.table)
library(gtools)
library(geosphere)
library(progress)


# Define API keys
# https://developers.google.com/places/web-service/get-api-key
# google_key <- "YOUR_API_KEY"
source("keys.R")


# Clean Manhattan lot locations from PLUTO data http://www.nyc.gov/html/dcp/html/bytes/dwn_pluto_mappluto.shtml
source("manhattan_lot_locations.R")


# Get manhattan froyo locations from Google Places API
source("froyo_locations_google.R")


# Calculate distances
source("distance_calculation.R")


# Save workspace
save.image("saved_workspace.RData")

