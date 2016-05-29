

# Find nearest froyo shop for each lot ################################################################

# Initialize an empty list to store the results
nearestList <- vector("list", nrow(manhattanLots))

pb <- progress::progress_bar$new(
  format = "  running [:bar] :percent eta: :eta",
  total = nrow(manhattanLots), clear = FALSE, width= 60)

for (i in 1:nrow(manhattanLots)) {
  
  # For lot i, calculate the great circle distance to each froyo shop in miles
  distance <- 0.000621371 * distCosine(p1=as.matrix(data.frame(manhattanLots$longitude[i], manhattanLots$latitude[i])),
                                    p2=as.matrix(data.frame(froyoData$geometry.location.lng, froyoData$geometry.location.lat)))
  
  # Filter to include only the nearest froyo shop
  temp.nearest <- cbind(froyoData, distance) %>%
    select(nearest_froyo_place_id=place_id,
           nearest_froyo_name=name,
           nearest_froyo_vicinity=vicinity,
           nearest_froyo_latitude=geometry.location.lat,
           nearest_froyo_longitude=geometry.location.lng,
           distance_to_nearest_froyo_miles=distance) %>%
    filter(distance==min(distance))
  
  # Join the lot info with the nearest froyo shop data
  temp.nearest <- cbind(manhattanLots[i, ], temp.nearest)[1,]
  
  # Insert the results into the nearestList
  nearestList[[i]] <- temp.nearest
  
  # Remove temp variables
  rm(distance, temp.nearest)
  
  pb$tick()
}

# Union the results to show, for each lot, the nearest froyo shop
lotNearestFroyo <- rbindlist(nearestList)

# Remove temp variables
rm(i, nearestList)

# Write lotNearestFroyo to csv
write.csv(lotNearestFroyo, file="datasets/lot_distance_to_froyo.csv", row.names=FALSE)

