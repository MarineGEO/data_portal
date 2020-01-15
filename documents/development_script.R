# Script used to test QA/QC functions/workflows independent of Shiny

library(tidyverse)
library(readxl)
library(purrr)

df <- all_data$`fish_seines_fish_seines_USA-VASB_2019-09-23.xlsx_USA-VASB`$sample_metadata

df_numeric <- select(df, depth_m, seine_mesh_size_mm, seine_width_m, seine_height_m, seine_distance_m)

results <- apply(df_numeric, 2, min)

rownames_to_column(as.data.frame(results), var="attribute_name")

apply(df_numeric, c(1,2), function(x){
  
})

map_dbl(df_numeric, min) %>%
  mutate(test = "column_min")

map_dbl(df_numeric, max) %>%
  mutate(test = "column_max")


rownames_to_column(as.data.frame(map_dbl(df_numeric, min)), var="attribute_name")

test <- setNames(as.data.frame(map_dbl(df_numeric, min)), "minimum") %>%
  rownames_to_column(var="attribute_name") %>%
  mutate(protocol = "fish_seines") %>%
  merge(protocol_structure, by=c("protocol", "attribute_name"), all.x=TRUE, all.y=FALSE) %>%
  select(protocol, attribute_name, minimum, minimum_warning)


test <- setNames(as.data.frame(map_dbl(df_numeric, min)), "maximum") %>%
  rownames_to_column(var="attribute_name") 


protocol_minimums <- protocol_structure %>%
  filter(protocol == "fish_seines" & sheet == "sample_metadata" & attribute_name %in% colnames(df_numeric))

df_numeric$depth_m <- c(-5, -8, 1)

# Numeric test 
df_numeric$depth_m < filter(protocol_minimums, attribute_name == "depth_m")$minimum_warning
which((test < 0) %in% 1) 
