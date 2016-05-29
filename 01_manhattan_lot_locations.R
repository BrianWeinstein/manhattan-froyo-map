

# Read in Manhattan PLUTO data http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_16v1.zip
manhattanLots <- read_csv(file="datasets/raw/Mn.csv")


# Remove unnecessary columns
manhattanLots <- manhattanLots %>%
  select(Block, Lot, Address, ZipCode, XCoord, YCoord) %>%
  na.omit()


# Project New York-Long Island State Plane coordinates to latitude/longitude
manhattanXY <- manhattanLots %>%
  select(XCoord, YCoord)

# http://www.spatialreference.org/ref/epsg/2263/
# http://www.spatialreference.org/ref/epsg/2263/proj4/
manhattanLatLon <- project(manhattanXY, 
                           '+proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs ', 
                           inverse=T) %>%
  as.data.frame() %>%
  select(latitude=y, longitude=x)


# Add latitude/longitude to manhattanLots data, create a unique id for each lot, and remove XY coords
manhattanLots <- cbind(manhattanLots, manhattanLatLon) %>%
  mutate(lot_id=paste(Block, Lot, sep="_")) %>%
  select(lot_id, Block, Lot, Address, ZipCode, latitude, longitude)

# Remove temp data
rm(manhattanXY, manhattanLatLon)


# Write manhattan lot data to csv
write.csv(manhattanLots, file="datasets/manhattan_lot_locations.csv", row.names=FALSE)

