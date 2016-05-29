

# Google Places Nearby Search API ################################################################################

BuildNearbySearchUrl <- function(keyword, latitude, longitude,
                                 rankby="prominence", radius, type=NULL,
                                 googleKey=google_key, nextPageToken=NULL){
  # Builds a URL to call the Google Places Nearby Search API.
  #
  # Args:  https://developers.google.com/places/web-service/search?hl=en#PlaceSearchRequests
  #   keyword: Search term
  #   latitude: The latitude around which to retrieve place information.
  #   longitude: The longitude around which to retrieve place information.
  #   rankby: Specifies the order in which the results are returned. Either "prominence" or "distance"
  #   radius: Defines the distance (in meters) within which to return place results.
  #   type: Restricts the results to places matching the specified types.
  #   googleKey: Your application's API key.
  #   nextPageToken: Returns the next 20 results from a previously run search.
  #
  # Returns:
  #   A URL to call the Google Places Nearby Search API.
  
  googleUrl <- paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=", google_key,
                      "&location=", latitude, ",", longitude, #40.777813,-73.970753",
                      "&rankby=", rankby,
                      ifelse(rankby=="distance", "", paste0("&radius=", radius)), 
                      "&keyword=", URLencode(as.character(keyword)),
                      ifelse(!is.null(type), paste0("&type=", type), ""),
                      ifelse(!missing(nextPageToken), paste0("&pagetoken=", nextPageToken), "")
  )
  
  return(googleUrl)
}




GetGooglePlacesNearbySearch <- function(keyword, latitude, longitude,
                                        rankby="prominence", radius, type=NULL,
                                        googleKey=google_key, pages=3){
  # Gets up to 60 (max) results from the Google Places Nearby Search API.
  #
  # Args:  https://developers.google.com/places/web-service/search?hl=en#PlaceSearchRequests
  #   keyword: Search term
  #   latitude: The latitude around which to retrieve place information.
  #   longitude: The longitude around which to retrieve place information.
  #   rankby: Specifies the order in which the results are returned. Either "prominence" or "distance"
  #   radius: Defines the distance (in meters) within which to return place results.
  #   type: Restricts the results to places matching the specified type.
  #   googleKey: Your application's API key.
  #   nextPageToken: Returns the next 20 results from a previously run search.
  #   pages: The number of pages of results to retrieve (1, 2, or 3); 20 results/page
  #
  # Returns:
  #   A dataframe of nearby places.
  
  # Initialize an empty list to store each page of results.
  resultsList <- list()
  
  # Initialize nextPageToken as NULL
  nextPageToken <- NULL
  
  # Set the number of pages to return (1, 2, or 3)
  numPages <- round(max(1, min(pages, 3)))
  
  # Request each page of results
  for(i in 1:numPages){
    
    message(paste0("    Fetching page ", i))
    
    # Build the URL
    googleUrl <- BuildNearbySearchUrl(keyword, latitude, longitude,
                                      rankby=rankby, radius, type,
                                      googleKey=googleKey,
                                      nextPageToken=nextPageToken)
    
    # Get data
    data <- GET(googleUrl)
    content <- content(data)
    contentList <- fromJSON(toJSON(content, digits=5), flatten=TRUE)
    
    message(paste0("        Status: ", contentList$status))
    
    # Store the next_page_token
    nextPageToken <- contentList$next_page_token
    
    # Insert the page of results into the resultsList
    resultsList[[i]] <- flatten(as.data.frame(contentList$results))
    
    # Pause for 2 seconds to wait for the next page to be available
    if(i<=2){Sys.sleep(2)}
  }
  
  # Union the results
  # results <- flatten(rbindlist(resultsList, use.names=TRUE, fill=TRUE))
  results <- flatten(do.call(smartbind, resultsList))
  
  # Return the results dataframe
  return(results)
  
}



# Get froyo shop locations ################################################################################


# Define manhattan region centers to use as search location centers
manhattanRegionCenters <- data.frame(region=c("manhattan lower", "manhattan midtown", "manhattan central", "manhattan upper"),
                                     latitude=c(40.727308, 40.758388, 40.785769, 40.834389),
                                     longitude=c(-73.996725, -73.983761, -73.962733, -73.941630))



# Initialize an empty list to store the results from each region center
froyoList <- list()

# Get up to 60 resuts for each region center
for(i in 1:nrow(manhattanRegionCenters)){
  
  message(paste0("REGION: ", manhattanRegionCenters$region[i]))
  
  froyoList[[i]] <- GetGooglePlacesNearbySearch(keyword="frozen yogurt nyc",
                                                latitude=manhattanRegionCenters$latitude[i],
                                                longitude=manhattanRegionCenters$longitude[i],
                                                radius=17000, rankby="distance", 
                                                #type="food",
                                                pages=3)
  
}

# Union and deduplicate the results
froyoData <- flatten(do.call(smartbind, froyoList)) # gtools::smartbind converts non-atomic-typed columns to characters.
froyoData <- froyoData[!duplicated(froyoData$place_id), ]

# Correct column classes
froyoData <- froyoData %>%
  mutate(geometry.location.lat=as.numeric(geometry.location.lat),
         geometry.location.lng=as.numeric(geometry.location.lng))


# Get the borough for the returned place_id 's

boroughList <- list() # Initialize an empty list to store the results

pb <- progress::progress_bar$new(
  format = "  running [:bar] :percent eta: :eta",
  total = nrow(froyoData), clear = FALSE, width= 60)

for(i in 1:nrow(froyoData)){
  
  place_id <- froyoData$place_id[i]
  
  placeDetails <- GET(paste0("https://maps.googleapis.com/maps/api/place/details/json?key=", google_key,
                             "&placeid=", place_id))
  placeAddress <- fromJSON(toJSON(content(placeDetails), digits = 5), flatten=TRUE)
  placeAddress <- placeAddress$result$address_components
  borough <- toString(filter(placeAddress, grepl("sublocality", types))$long_name)
  
  boroughList[[i]] <- data.frame(place_id=place_id, borough=borough, stringsAsFactors=FALSE)
  
  rm(place_id, placeDetails, placeAddress, borough)
  
  pb$tick()
}

# Union the results
boroughData <- rbindlist(boroughList)
boroughData$borough[grepl("^$", boroughData$borough)] <- NA


# Filter the results to only include shops in Manhattan
froyoData <- left_join(froyoData, boroughData, by="place_id") %>%
  filter(borough == "Manhattan")


# Remove temp variables
rm(manhattanRegionCenters, froyoList, boroughList, boroughData, i)


# Write froyoData to csv
write.csv(froyoData, file="datasets/froyo_locations_google.csv", row.names=FALSE)


