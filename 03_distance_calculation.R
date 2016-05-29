

"https://maps.googleapis.com/maps/api/distancematrix/json?key=google_key&origins=40.7271,-73.99577|40.74286,-74.00003&destinations=40.73805,-73.99674|40.79284,-73.97269|40.80537,-73.96621&mode=walking"



# 
# # Define a distance function ################################################################
# 
# # Manhattan distance (with rotation)
# ManDist <- function(latitude1, longitude1, latitude2, longitude2) {
#   # Calculates the Manhattan distance between two (latitude, longitude)
#   # points, accounting for Manhattan block rotation.
#   #
#   # Args:
#   #   latitude1: The latitude of point 1
#   #   longitude1: The longitude of point 1
#   #   latitude2: The latitude of point 2
#   #   longitude2: The longitude of point 2
#   #
#   # Returns:
#   #   A relative distance metric (latitude difference + longitude differencedifference) for
#   #   the two points, accounting for Manhatan block rotation.
#   
#   theta <- -0.51 # NYC rotation (radians)
#   
#   newLong1 <- ((longitude1 * cos(theta)) + (latitude1 * sin(theta)))
#   newLat1 <- ((latitude1 * cos(theta)) - (longitude1 * sin(theta)))
#   
#   newLong2 <- ((longitude2 * cos(theta)) + (latitude2 * sin(theta)))
#   newLat2 <- ((latitude2 * cos(theta)) - (longitude2 * sin(theta)))
#   
#   abs(newLong1 - newLong2) + abs(newLat1 - newLat2)
#   
# }
# 
# 
# 
# # Calculate distance in miles between two points
# # adapted from https://conservationecology.wordpress.com/2013/06/30/distance-between-two-points-in-r/
# earth.dist <- function (long1, lat1, long2, lat2){
#   
#   theta <- -0.51 # NYC rotation (radians)
#   
#   long1 <- ((long1 * cos(theta)) + (lat1 * sin(theta)))
#   lat1 <- ((lat1 * cos(theta)) - (long1 * sin(theta)))
#   
#   long2 <- ((long2 * cos(theta)) + (lat2 * sin(theta)))
#   lat2 <- ((lat2 * cos(theta)) - (long2 * sin(theta)))
#   
#   
#   rad <- pi/180
#   a1 <- lat1 * rad
#   a2 <- long1 * rad
#   b1 <- lat2 * rad
#   b2 <- long2 * rad
#   dlon <- b2 - a2
#   dlat <- b1 - a1
#   a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
#   c <- 2 * atan2(sqrt(a), sqrt(1 - a))
#   R <- 3956.5466780874689903
#   d <- R * c
#   return(d)
# }



# Find nearest froyo shop for each lot ################################################################

# Initialize an empty list to store the results
nearestList <- vector("list", nrow(manhattanLots))

pb <- progress::progress_bar$new(
  format = "  running [:bar] :percent eta: :eta",
  total = nrow(manhattanLots), clear = FALSE, width= 60)

for (i in 1:nrow(manhattanLots)) {
  
  # For lot i, calculate the distance to each froyo shop
  # distance <- ManDist(manhattanLots$latitude[i], manhattanLots$longitude[i],
  #                     froyoData$geometry.location.lat, froyoData$geometry.location.lng) 
  # distance <- earth.dist(lat1=manhattanLots$latitude[i], long1 = manhattanLots$longitude[i],
  #                        lat2=froyoData$geometry.location.lat, long2 = froyoData$geometry.location.lng)
  
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



