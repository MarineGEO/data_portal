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

# seq along columns :
# Numeric test 
# Which values are less than the minimum
results <- df_numeric$depth_m < filter(protocol_minimums, attribute_name == "depth_m")$minimum_warning
which(results %in% TRUE) 
if(length(which(results %in% TRUE)))
  
  
df_numeric$depth_m < filter(protocol_minimums, attribute_name == "depth_m")$maximum_warning
which((test < 0) %in% 1) 


paste(as.character(df_numeric[minimum_results, "depth_m"]), collapse=", ")

####
protocol_minmax <- protocol_structure %>%
  filter(protocol == current_protocol & sheet == sheet_name & attribute_name %in% colnames(testing_df))

for(column in testing_df){
  
  ## ... Minimum Test ####
  # Find the index for any values that are less than the attribute's minimum
  min_invalid_row_index <- which(testing_df[column] < filter(protocol_minmax, attribute_name == column)$minimum_warning 
        %in% 1) 
  # If there are invalid values:
  if(length(min_invalid_row_index) > 0){
    # Extract invalid value at each index
    invalid_values <- pull(testing_df[column])[min_invalid_row_index]
    
    numeric_results <- setNames(as.data.frame(paste(min_invalid_row_index, collapse = ", ")), "row_numbers") %>%
      mutate(column_name = column,
             sheet_name = sheet_name,
             protocol = current_protocol,
             test = "Invalid minimum value",
             filename = filenames[i],
             values = paste(invalid_values, collapse=", ")) %>%
      select(test, filename, protocol, sheet_name, column_name, row_numbers, values) %>%
      bind_rows(numeric_results)
  }
  
  ## ... Maximum Test ####
  # Find the index for any values that are greater than the attribute's maximum
  max_invalid_row_index <- which(testing_df[column] > filter(protocol_minmax, attribute_name == column)$maximum_warning 
                             %in% 1) 
  # If there are invalid values:
  if(length(max_invalid_row_index) > 0){
    # Extract invalid value at each index
    invalid_values <- pull(testing_df[column])[max_invalid_row_index]
    
    numeric_results <- setNames(as.data.frame(paste(invalid_row_index, collapse = ", ")), "row_numbers") %>%
      mutate(column_name = column,
             sheet_name = sheet_name,
             protocol = current_protocol,
             test = "Invalid maximum value",
             filename = filenames[i],
             values = paste(invalid_values, collapse=", ")) %>%
      select(test, filename, protocol, sheet_name, column_name, row_numbers, values) %>%
      bind_rows(numeric_results)
  }
  
}
