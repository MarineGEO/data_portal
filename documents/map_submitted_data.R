## Map Sampling Locations in Submitted Protocols

library(tidyverse)
library(maps)
library(readxl)

## Set up test data submission ##########

# function that will be used to read in all sheets from a xlsx protocol
readAllExcelSheets <- function(filename) {
  # list the sheet names for each excel file
  sheets <- excel_sheets(filename)
  
  # read in all sheets to a list
  x <- lapply(sheets, function(X) read_excel(filename, sheet = X))

  # assign the sheet names
  names(x) <- sheets
  
  return(x)
}

# create a nested list of the submitted files
protocol_files <- list.files("documents/test_data/test_protocols/", full.names = TRUE)
protocol_names <- c("seines", "predation", "seagrass_density", "seagrass_epifauna",
                    "seagrass_shoots", "enviro_monitoring")

all_protocols <- list()

for (k in 1:length(protocol_files)) {
  # read in all sheets per file
  mysheets <- readAllExcelSheets(protocol_files[k])
  
  # append the all protocols list with each file in the submission
  all_protocols[[k]] <- mysheets
  k <- k + 1
}
# name the protocols accordingly
names(all_protocols) <- protocol_names


## Compile latitude/longitude from protocols ##########

coords <- data.frame()

for (i in 1:length(all_protocols)){
  protocol_name <- names(all_protocols[i])
  
  for (j in 1:length(names(all_protocols[[i]]))){
    sheet_name <- names(all_protocols[[i]][j])
    
    if (any(grepl("latitude|longitude", names(all_protocols[[i]][[j]])))){
      
      if (any(grepl("begin_decimal|end_decimal", names(all_protocols[[i]][[j]])))){
        # separate transect start and end coordinates
        begin_coords <- select(all_protocols[[i]][[j]], matches("begin_decimal")) %>%
          rename(latitude = transect_begin_decimal_latitude,
                 longitude = transect_begin_decimal_longitude)
        end_coords <- select(all_protocols[[i]][[j]], matches("end_decimal")) %>%
          rename(latitude = transect_end_decimal_latitude,
                 longitude = transect_end_decimal_longitude)
        # bind lat/lon pairs together
        temp_coords <- bind_rows(begin_coords, end_coords) %>%
          mutate(protocol = protocol_name,
                 sheet = sheet_name)
        
      } else if (any(grepl("point_decimal", names(all_protocols[[i]][[j]])))){
        # isolate lat/lon pairs
        temp_coords <- select(all_protocols[[i]][[j]], matches("latitude|longitude")) %>%
          rename(latitude = sample_point_decimal_latitude,
                 longitude = sample_point_decimal_longitude) %>%
          mutate(protocol = protocol_name,
                 sheet = sheet_name)
      } else {
        print(str_c("Unidentified lat/long columns in the", protocol_name, 
                    "protocol and", sheet_name, "sheet", sep = " "))
      }
      # compile all lat/lon pairs in submission 
      coords <- bind_rows(coords, temp_coords) %>%
        select(protocol, sheet, latitude, longitude) %>%
        drop_na(latitude)
    }
  }
}

# use protocol structure table to generalize the prior step
# protocol_structure <- read_csv("data_portal_prototype/data/protocol_structure.csv")

# Plot all coords in protocol submission ##########

worldmap <- map_data("world")
usa_map <- map_data("usa")

# plot sampling points in one map
ggplot() +
  geom_polygon(data = usa_map,
           mapping = aes(long, lat, group = group),
           fill = "grey50") +
  geom_point(data = coords,
           aes(x = longitude,
               y = latitude, col = protocol)) +
  # scale_x_continuous(limits = c(-79, -75)) +
  # scale_y_continuous(limits = c(37.5, 39)) +
  coord_quickmap() +
  xlab("Longitude (Decimal)") + ylab("Latitude (Decimal)")
# figure out how to scale to the points resonably
# use worldmap if the points are outside the US

# ggplot() +
#   geom_polygon(data = worldmap, aes(long, lat, group = group), 
#                fill = "grey50", color = "white")

# plotting transects
# test <- all_protocols[[2]][[2]]
# 
# test_coords <- test %>% 
#   select(matches("latitude|longitude")) %>%
#   mutate(transect_end_decimal_longitude = transect_begin_decimal_longitude + 0.2,
#          transect_end_decimal_latitude = transect_begin_decimal_latitude + 0.2)
# how do I pair transect start and end coords most effectively
# for transects, explore geom_path, geom_line, geom_pointrange, geom_segment

  # ggplot() +
  #   geom_map(data = worldmap, map = worldmap, mapping = aes(map_id = region)) +
  #   geom_line() +
  #   # only works if 
  #   geom_segment(data = test_coords, 
  #                aes(x = transect_begin_decimal_longitude, 
  #                    y = transect_begin_decimal_latitude,
  #                    xend = transect_end_decimal_longitude, 
  #                    yend = transect_end_decimal_latitude),
  #                color = "blue") +
  #   scale_x_continuous(expand = c(0.3, 0.3)) +
  #   scale_y_continuous(expand = c(0.3, 0.3)) +
  #   xlab("Longitude (Decimal)") + ylab("Latitude (Decimal)")