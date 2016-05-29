

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


# 
# 
# lotNearestFroyo %>%
#   arrange(-distance) %>%
#   left_join(x=as.data.frame(.),y=select(froyoData, froyo_id=place_id, vicinity), by="froyo_id") %>%
#   mutate(directions=paste0(Address, " to ", vicinity)) %>%
#   View()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# lotNearestFroyo.sample <- sample_n(lotNearestFroyo, size=4000, replace=FALSE)
# 
# # color each lot
# ggmap(map.google.terrain, extent="device", darken = c(0.0, "black")) +
#   geom_point(data=lotNearestFroyo.sample, aes(x=longitude, y=latitude, color=distance^(1/2.5)), alpha=0.05, size=4) +
#   scale_color_gradientn(colors=c("green", "yellow", "red")) +
#   geom_point(data=froyoData, aes(x=geometry.location.lng, y=geometry.location.lat), size=1, color="black")
#ggsave(filename="lot_distances.png")
# 
# 
# # density around froyo
# ggmap(map.google, extent="panel", darken = c(0.0, "black")) +
#   #stat_density2d(data = froyoPlotData, aes(x=longitude, y=latitude, fill = ..level.., alpha = ..level..), size = .01, bins = 100, geom = "polygon") +
#   stat_density2d(data = froyoPlotData, aes(x=longitude, y=latitude, fill = ..level..), alpha = 0.01, size = .01, bins = 100, geom = "polygon") +
#   geom_density2d(data=froyoPlotData, aes(x=longitude, y=latitude), size = 0.3, bins=20) +
#   scale_fill_gradient(low = "green", high = "red") + 
#   scale_alpha(range = c(0, 0.1), guide = FALSE) +
#   geom_point(data=froyoPlotData, aes(x=longitude, y=latitude), size=2, color="black")
# 
# 



