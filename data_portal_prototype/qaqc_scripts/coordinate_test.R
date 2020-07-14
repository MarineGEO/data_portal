## Function to test coordinates

# library(tidyverse)

# Input: Coords scraped from protocols

# Testing:
# coords are numeric 
# coords in decimal degree format

test_coords <- read_csv("documents/test_data/test_coords.csv")

# assign segment IDs 
test_coords$segment_id <- seq(from = 1, to = nrow(test_coords))

# gather coordinates into long-form
reshape_coords <- test_coords %>%
  pivot_longer(cols = c(latitude_1, longitude_1, latitude_2, longitude_2), 
               names_to = c(".value", "coord_order"), names_sep = "_") %>%
  select(-coord_order)

# Test numeric ####
if(is.numeric(reshape_coords$latitude) | is.numeric(reshape_coords$longitude)){
  print("Lat and Lon are not numeric")
}

## Test expected range ####

# Latitudes range from -90 to 90, and longitudes range from -180 to 180
# does any lat/long value fall outside of the expected range? TRUE = yes
if(any(!between(reshape_coords$latitude, -90, 90) | between(reshape_coords$longitude, -180, 180),
    na.rm = TRUE)){
  print("There is a lat/long value which falls outside of the expected range")
}
  
  
  
  