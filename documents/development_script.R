# Script used to test QA/QC functions/workflows independent of Shiny

library(tidyverse)
library(readxl)

# Format metadata to be more machine-readable
# Turn to wide form, remove first row
protocol_metadata <- read_xlsx("./documents/test.xlsx", sheet = "protocol_metadata", col_names = c("category", "response"), skip=1) %>%
  spread(category, response) %>%
  mutate(data_entry_date = as.Date(as.numeric(data_entry_date), origin = "1899-12-30"))

sample_metadata <- read_xlsx("./documents/test.xlsx", sheet = "sample_metadata")

# file name will be [Protocol]_[MarineGEO site code]_[data entry date in YYYY-MM-DD format]
file_name <- paste(protocol_metadata$protocol_name, first(sample_metadata$site_code), protocol_metadata$data_entry_date, sep="_")

# Make sure there's only one site code in sample metadata 
# Scan directory to make sure there isn't a shared name

# If excel sheet was done on Mac, date may be wrong since origin is different
