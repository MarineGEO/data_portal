# Script used to test QA/QC functions/workflows independent of Shiny

library(tidyverse)
library(readxl)
library(purrr)

df <- all_data$`fish_seines_fish_seines_USA-VASB_2019-09-23.xlsx_USA-VASB`$sample_metadata

df_numeric <- select(df, depth_m, seine_mesh_size_mm, seine_width_m, seine_height_m, seine_distance_m)



coords <- c("34  °32'1.3632''N", "98°36'47.8116''W")
coords <- c(23423, 235235, 234234, 23434)
coords <- c("43°46′50.5″N",
            "79°24′53″W",
            "43°46′06″N", 
            "79°24′46″W")

test <- as.data.frame(coords)

# If coordinates is not numeric, assume its in dms and try to convert:
if(
!tryCatch({
  as.numeric(test)
  TRUE
},
warning = function(w){
  FALSE
},
error = function(e){
  FALSE
})){
  tryCatch({
    # Remove whitespace and count the number of non-numeric characters  
    coords_conversion <- mutate(test, coords = gsub(" ", "", coords)) %>%
      mutate(chr_count = str_count(coords, "[^0-9.-]"))
    
    # If count >= 3, try to convert to dd 
    if(all(coords_conversion$chr_count >= 3)){
      coords_conversion <- coords_conversion %>%
        separate(coords, into = c("d", "min", "sec"), sep="[^0-9.-]", extra="drop") %>%
        mutate(dd = as.numeric(d) + (as.numeric(min)/60) + (as.numeric(sec)/3600))
    # Convert to (-) based on site code
    }
    
  },
  warning = function(w){
    
  },
  error = function(e){
    
  })
}


# First remove any whitespace

# Separate into component parts