## Map Sampling Locations in Submitted Protocols

library(tidyverse)
library(maps)
library(readxl)

## Set up test data submission ####

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

## Identify latitude/longitude in protocols

# fish trawls, beach seins, other

protocol_structure <- read_csv("data_portal_prototype/data/protocol_structure.csv")

test <- all_protocols[[6]][[2]]

test_coords <- test %>% 
  select(matches("latitude|longitude"))
  mutate(transect_end_decimal_longitude = transect_begin_decimal_longitude + 0.2,
         transect_end_decimal_latitude = transect_begin_decimal_latitude + 0.2)
# how do I pair transect start and end coords most effectively
# for transects, explore geom_path, geom_line, geom_pointrange, geom_segment

worldmap <- map_data("world")

# ggplot() +
#   geom_polygon(data = worldmap, aes(long, lat, group = group), 
#                fill = "grey50", color = "white")

ggplot() +
  geom_map(data = worldmap, map = worldmap, mapping = aes(map_id = region)) +
  # geom_point(data = test_coords, 
  #            aes(x = sample_point_decimal_longitude, 
  #                y = sample_point_decimal_latitude))
  geom_line() +
  geom_segment(data = test_coords, 
               aes(x = transect_begin_decimal_longitude, 
                   y = transect_begin_decimal_latitude,
                   xend = transect_end_decimal_longitude, 
                   yend = transect_end_decimal_latitude),
               color = "blue") +
  coord_map(xlim = c(test_coords$transect_begin_decimal_longitude - 1,
                     test_coords$transect_begin_decimal_longitude + 1))



