# Script used to test QA/QC functions/workflows independent of Shiny

library(tidyverse)
library(readxl)

sample_metadata <- read_excel(paste("./documents/secrets", "fish_seines_USA-VASB_2019-09-23.xlsx", sep="/"), 
                              sheet = "sample_metadata", 
                              na = c("NA", "This cell will autocalculate", "N/A"))

data <- read_excel(paste("./documents/secrets", "fish_seines_USA-VASB_2019-09-23.xlsx", sep="/"), 
                   sheet = "sample_data", 
                   na = c("NA", "This cell will autocalculate", "N/A"))

test <- anti_join(data, sample_metadata, by=c("sample_collection_date", "site_code", "location_name", "transect"))
test <- anti_join(sample_metadata, data, by=c("sample_collection_date", "site_code", "location_name", "transect"))

# First do anti_join with data on left side
# If there are rows, combine IDs for alert

# Second, do opposite anti join
# Combine IDs for alerts

# If none of these provide alerts, done
# But if so, run tests for each ID category
# Make sure all data sheets are found in sample metadata, check for any NAs


test <- unite(data, id)

## Test 1 ##
# If ID in data sheet but missing from sample metadata: Row from data
# If ID in sample metadata but missing from data sheet: Nothing

## Test 2 ##
# If ID in data sheet but missing from sample metadata: Nothing
# If ID in sample metadata but missing from data sheet: Row from sample metadata

id <- "transect"
test <- rowid_to_column(data, "row") %>%
  anti_join(sample_metadata, by=id) %>%
  select(id, row)

results <- test %>%
  gather("column_name", "value", -row)%>%
  group_by(column_name, value) %>%
  summarize(row_numbers = paste(row, collapse=", ")) %>%
  mutate(sheet_name = sheet_name,
         protocol = current_protocol,
         test = "Test1: Data ID relationships",
         filename = filenames[i]) %>%
  select(test, filename, protocol, sheet_name, column_name, value, row_numbers) 


