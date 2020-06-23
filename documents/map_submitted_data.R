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

# use protocol structure table to generalize the prior step
# the lat/lon columns have been given the id_variable "3"
protocol_structure <- read_csv("data_portal_prototype/data/protocol_structure.csv")

# isolate the coordinate-containing columns
coord_cols <- protocol_structure %>% 
  select(attribute_name, id_variable) %>%
  filter(id_variable == 3) %>%
  distinct() %>%
  pull(attribute_name)

## Compile latitude/longitude from protocols ##########

coords <- data.frame()

for (i in 1:length(all_protocols)){
  # save protocol name
  protocol_name <- names(all_protocols[i])
  
  for (j in 1:length(names(all_protocols[[i]]))){
    # save protocol sheet name
    sheet_name <- names(all_protocols[[i]][j])
    
    if (any(names(all_protocols[[i]][[j]]) %in% coord_cols)){
      # save coordinate-containing sheet to object 
      temp_sheet <- all_protocols[[i]][[j]]
      # isolate coordinate cols from the sheet
      temp_cols <- temp_sheet[, which(names(temp_sheet) %in% coord_cols)]

      if (ncol(temp_cols) == 4){
        # assuming that the order of the columns is correct...
        # generalize the col names
        names(temp_cols) <- c("latitude_1", "longitude_1", "latitude_2", "longitude_2")
        
        # bind lat/lon pairs with site and transect info
        temp_coords <- temp_sheet %>% select(site_code, transect) %>%
          bind_cols(., temp_cols) %>%
          mutate(protocol = protocol_name,
                 sheet = sheet_name)
        
      } else if (ncol(temp_cols) == 2){
        # generalize the column names (same assumption about col order)
        names(temp_cols) <- c("latitude_1", "longitude_1")
        
        # bind lat/lon pairs with site and transect info
        temp_coords <- temp_sheet %>% select(site_code, transect) %>%
          bind_cols(., temp_cols) %>%
          mutate(protocol = protocol_name,
                 sheet = sheet_name)
        
      } else {
        print(str_c("Unidentified lat/long columns in the", protocol_name, 
                    "protocol and", sheet_name, "sheet", sep = " "))
      }
      # compile all lat/lon pairs in submission 
      coords <- bind_rows(coords, temp_coords) %>%
        select(protocol, sheet, site_code, transect, everything()) %>%
        drop_na(latitude_1)
    }
  }
}

# Plot all coords in protocol submission ##########

worldmap <- map_data("world")
usa_map <- map_data("usa")

# plot sampling points in one map
ggplot() +
  # use worldmap if the points are outside the US
  geom_polygon(data = usa_map,
           mapping = aes(long, lat, group = group),
           fill = "grey50") +
  geom_point(data = coords,
           aes(x = longitude_1,
               y = latitude_1, col = protocol)) +
  # figure out how to scale to the points resonably
  # scale_x_continuous(limits = c(-79, -75)) +
  # scale_y_continuous(limits = c(37.5, 39)) +
  coord_quickmap() +
  xlab("Longitude (Decimal)") + ylab("Latitude (Decimal)")

# ggplot() +
#   geom_polygon(data = worldmap, aes(long, lat, group = group), 
#                fill = "grey50", color = "white")

# plotting transects
transect_coords <- coords %>% drop_na(latitude_2)

# for transects, explore geom_path, geom_line, geom_pointrange, geom_segment
  ggplot() +
    geom_map(data = worldmap, map = worldmap, mapping = aes(map_id = region)) +
    geom_line() +
    geom_segment(data = transect_coords,
                 aes(x = longitude_1,
                     y = latitude_1,
                     xend = longitude_2,
                     yend = latitude_2),
                 color = "blue") +
    scale_x_continuous(expand = c(0.003, 0.003)) +
    scale_y_continuous(expand = c(0.003, 0.003)) +
    xlab("Longitude (Decimal)") + ylab("Latitude (Decimal)")
  
# combine points and transect in one map
  ggplot() +
    # use worldmap if the points are outside the US
    geom_polygon(data = usa_map,
                 mapping = aes(long, lat, group = group),
                 fill = "grey50") +
    geom_point(data = coords,
               aes(x = longitude_1,
                   y = latitude_1, col = protocol)) +
    geom_line() +
    geom_segment(data = transect_coords,
                 aes(x = longitude_1,
                     y = latitude_1,
                     xend = longitude_2,
                     yend = latitude_2),
                 color = "blue") +
    coord_quickmap() +
    xlab("Longitude (Decimal)") + ylab("Latitude (Decimal)")
  
# Experiment with leaflet
library(leaflet)

  # define palette 
  pal <- colorFactor(palette = "Set3", 
                     coords$protocol)
  
  # plot coordinates
  leaflet(coords) %>% 
    addProviderTiles(providers$CartoDB) %>%
    # addTiles() %>%
    addCircleMarkers(lng = ~longitude_1, lat = ~latitude_1, radius = 5, color = ~pal(protocol)) %>%
    addLegend(pal = pal, values = ~protocol)
  
  # plot transects
  transect_df <- with(transect_coords,
                data.frame(group = c("A", "B"),
                           protocol = protocol,
                           lat = c(latitude_1, latitude_2),
                           long = c(longitude_1, longitude_2))
  )
  
  leaflet()%>%
    addProviderTiles(providers$CartoDB) %>%
    # problem: Polylines will connect the transects
    addPolylines(data = transect_df, lng = ~long, lat = ~lat, group = ~group, 
                 color = ~pal(protocol), weight = 1)
      